package com.sootsafe.firebase.subscriber

import java.io.InputStream

import com.google.auth.oauth2.GoogleCredentials
import com.google.cloud.firestore._
import com.google.firebase.cloud.FirestoreClient
import com.google.firebase.{FirebaseApp, FirebaseOptions}
import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import org.json4s.DefaultFormats

import scala.concurrent.Channel
import scala.util.{Failure, Success, Try}


trait Subscriber {
  def subscribe[T](db: Firestore, serializer: PartialFunction[String, T], outputChannel: Channel[(T, DocumentReference)]): Unit
}

object DefaultSubscriber extends Subscriber {

  import scala.collection.JavaConversions._

  private val conf = ConfigFactory.load()

  private def createEventListener[T](messageChannel: Channel[(T, DocumentReference)],
                                  db: Firestore,
                                  serializer: PartialFunction[String, T]): EventListener[QuerySnapshot] with Object {
    def onEvent(snapshot: QuerySnapshot, error: FirestoreException): Unit
  } = new EventListener[QuerySnapshot] {

    override def onEvent(snapshot: QuerySnapshot, error: FirestoreException): Unit = {
      if (error != null) {
        System.err.println("Listen failed: " + error)
        throw new Exception(s"Listen to firebase failed", error)
      } else {
        // A new reference to a request has been detected
        // Fetch the data in that spot, and send it to singleDocumentUpdateToJson
        val changes = snapshot.getDocumentChanges.iterator().toSeq
        changes.map {
          change: DocumentChange =>
            Try(change.getDocument.getData.get("id")) match {
              case Success(id: String) if change.getType == DocumentChange.Type.ADDED =>
                val request = db.collection("releaseRate").document(id).get().get()
                singleDocumentUpdateToJson(request) match {
                  case Success(json) =>
                    val t = serializer(json)
                    messageChannel.write(t, request.getReference)

                    // Delete the original reference to the request data, so that we don't pick it up again if we restart the backend
                    change.getDocument.getReference.delete()
                  case Failure(e) => ???
                }
              case x =>
                throw new Exception(s"Unexpected data when looking for atex requests: $x")
            }
        }
      }
    }
  }

  def subscribe[T](db: Firestore, serializer: PartialFunction[String, T], messageChannel: Channel[(T, DocumentReference)]): Unit = {
    val docRef = db.collection("releaseRateRequests")
    //    docRef.addSnapshotListener(createSnapshotListener(messageChannel))
    docRef.addSnapshotListener(createEventListener(messageChannel, db, serializer))
  }

  private def singleDocumentUpdateToJson(snapshot: DocumentSnapshot): Try[String] = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    if (snapshot.exists()) {
      System.out.println("Added data: " + snapshot.getData + ", Reference: " + snapshot.getReference.getPath)
      val gson = new Gson()
      Success(gson.toJson(snapshot.getData))
    } else {
      Failure(new Exception(s"Document with id ${snapshot.getId} does not exist"))
    }
  }

  private lazy val getServiceAccountStream: InputStream = {
    val keyFilePath = conf.getString("firebase.keyFile")
    getClass.getClassLoader.getResourceAsStream(keyFilePath)
  }

  def database(serviceAccountStream: InputStream = getServiceAccountStream): Firestore = {
    val storageBucket = conf.getString("firebase.storageBucket")
    val credentials = GoogleCredentials.fromStream(serviceAccountStream)
    val options = new FirebaseOptions.Builder()
      .setStorageBucket(storageBucket)
      .setCredentials(credentials)
      .build
    FirebaseApp.initializeApp(options)

    FirestoreClient.getFirestore
  }

}

