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


object Subscriber {

  def subscribe(db: Firestore, messageChannel: Channel[String]): Unit = {

    import org.json4s.jackson.Serialization.write

    import scala.collection.JavaConversions._
    implicit val formats: DefaultFormats.type = DefaultFormats

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
            (change.getType, change.getDocument.exists) match {
              case (DocumentChange.Type.ADDED, true) =>
                System.out.println("Added data: " + change.getDocument.getData + ", " + change.getType)
//                val fakeMap: java.util.Map[String, AnyRef] = Map("FakeResult" -> UUID.randomUUID())
//                docRef.document("report").set(fakeMap)
                val gson = new Gson()
                val json = gson.toJson(change.getDocument.getData)
//                val json = write(change.getDocument.getData)
                messageChannel.write(json)
              case (DocumentChange.Type.MODIFIED, _) =>
                System.out.println("Modified data: " + change.getDocument.getData + ", " + change.getType)
                val json = write(change.getDocument.getData)
                messageChannel.write(json)
              case (changeType, _) =>
                System.out.print(s"ChangeType $changeType not implemented/supported")
            }
          }
        }
      }
    })
  }

  // FIXME: Setting should go here I guess
  private val serviceAccountURL: URL = getClass.getResource("/keys/SootSafeAppTest.json")

  def database(serviceAccountFileURL: URL = serviceAccountURL): Firestore = {
    // Use a service account// Use a service account
    val serviceAccount = new FileInputStream(serviceAccountFileURL.getFile)
    val credentials = GoogleCredentials.fromStream(serviceAccount)
    val options = new FirebaseOptions.Builder().setCredentials(credentials).build
    FirebaseApp.initializeApp(options)

    FirestoreClient.getFirestore
  }

}

