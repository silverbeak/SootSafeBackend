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

  def subscribe(db: Firestore, messageChannel: Channel[(String, DocumentReference)]): Unit = {

    import scala.collection.JavaConversions._

    val docRef = db.collection("releaseRate")
    docRef.addSnapshotListener(new EventListener[QuerySnapshot]() {
      override def onEvent(snapshot: QuerySnapshot, e: FirestoreException): Unit = {
        if (e != null) {
          System.err.println("Listen failed: " + e)
          throw new Exception(s"Listen to firebase failed", e)
        } else {
          for {
            change <- snapshot.getDocumentChanges
          } {
            singleDocumentUpdateToJson(messageChannel, change) match {
              case Success(json) =>
                messageChannel.write(json, change.getDocument.getReference)

              case Failure(e) =>
                ???
            }
          }
        }
      }
    })
  }

  private def singleDocumentUpdateToJson(messageChannel: Channel[(String, DocumentReference)], change: DocumentChange): Try[String] = {
    import org.json4s.jackson.Serialization.write
    implicit val formats: DefaultFormats.type = DefaultFormats

    (change.getType, change.getDocument.exists) match {
      case (DocumentChange.Type.ADDED, true) =>
        System.out.println("Added data: " + change.getDocument.getId + ", Reference: " + change.getDocument.getReference.getPath)
        val gson = new Gson()
        Success(gson.toJson(change.getDocument.getData))

      case (DocumentChange.Type.MODIFIED, _) =>
        System.out.println("Modified data: " + change.getDocument.getData + ", " + change.getType)
        Success(write(change.getDocument.getData))

      case (changeType, _) =>
        Failure(new Exception(s"ChangeType $changeType not implemented/supported"))
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

