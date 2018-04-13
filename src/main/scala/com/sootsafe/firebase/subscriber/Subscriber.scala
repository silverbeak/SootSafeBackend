package com.sootsafe.firebase.subscriber

import java.io.FileInputStream
import java.net.URL

import com.google.auth.oauth2.GoogleCredentials
import com.google.cloud.firestore._
import com.google.firebase.cloud.FirestoreClient
import com.google.firebase.{FirebaseApp, FirebaseOptions}
import com.google.gson.Gson
import org.json4s.DefaultFormats

import scala.concurrent.Channel
import scala.util.{Failure, Success, Try}


object Subscriber {

  private def createEventListener(messageChannel: Channel[(String, DocumentReference)],
                                  db: Firestore): EventListener[DocumentSnapshot] = new EventListener[DocumentSnapshot] {

    override def onEvent(snapshot: DocumentSnapshot, error: FirestoreException): Unit = {
      if (error != null) {
        System.err.println("Listen failed: " + error)
        throw new Exception(s"Listen to firebase failed", error)
      } else {
        // A new reference to a request has been detected
        // Fetch the data in that spot, and send it to singleDocumentUpdateToJson
        Try(snapshot.getData.get("id")) match {
          case Success(id: String) =>
            val request = db.collection("releaseRate").document(id).get().get()
            singleDocumentUpdateToJson(messageChannel, request) match {
              case Success(json) =>
                messageChannel.write(json, request.getReference)

                // Delete the original reference to the request data, so that we don't pick it up again if we restart the backend
                snapshot.getReference.delete()
              case Failure(e) => ???
            }
          case _ => ???
        }
      }
    }
  }

  def subscribe(db: Firestore, messageChannel: Channel[(String, DocumentReference)]): Unit = {
    val docRef = db.collection("releaseRateRequests").document("requests")
    //    docRef.addSnapshotListener(createSnapshotListener(messageChannel))
    docRef.addSnapshotListener(createEventListener(messageChannel, db))
  }

  private def singleDocumentUpdateToJson(messageChannel: Channel[(String, DocumentReference)], snapshot: DocumentSnapshot): Try[String] = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    if (snapshot.exists()) {
      System.out.println("Added data: " + snapshot.getData + ", Reference: " + snapshot.getReference.getPath)
      val gson = new Gson()
      Success(gson.toJson(snapshot.getData))
    } else {
      Failure(new Exception(s"Document with id ${snapshot.getId} does not exist"))
    }
  }

  // FIXME: Setting should go here I guess
  private val serviceAccountURL: URL = getClass.getResource("/keys/SootSafeAppTest.json")

  def database(serviceAccountFileURL: URL = serviceAccountURL): Firestore = {
    // Use a service account// Use a service account
    val serviceAccount = new FileInputStream(serviceAccountFileURL.getFile)
    val credentials = GoogleCredentials.fromStream(serviceAccount)
    val options = new FirebaseOptions.Builder()
      .setStorageBucket("sootsafe-app-test.appspot.com")
      .setCredentials(credentials)
      .build
    FirebaseApp.initializeApp(options)

    FirestoreClient.getFirestore
  }

}

