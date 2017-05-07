package tryp.mongo

import reactivemongo.bson._
import reactivemongo.core.commands._
import reactivemongo.api.{SerializationPack, BSONSerializationPack}
import reactivemongo.api.commands.{CollectionCommand, CommandWithResult}

case class MapReduce(
  mapFunction: String,
  reduceFunction: String,
  query: Option[BSONDocument] = None
)
extends CollectionCommand
with CommandWithResult[BSONDocument]
