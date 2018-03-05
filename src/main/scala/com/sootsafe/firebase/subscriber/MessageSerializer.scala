package com.sootsafe.firebase.subscriber

import com.google.protobuf.Message

import scala.reflect.ClassTag


object MessageSerializer {
  def serializer[T <: Message : ClassTag](json: String, builder: Message.Builder): T = {
    com.google.protobuf.util.JsonFormat.parser.merge(json, builder)
    builder.build match {
      case x: T => x
      case x: Message =>
        throw new Exception(s"Tried to serialize message, but got ${x.getClass.getName}")
    }
  }
}
