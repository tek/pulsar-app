package tryp.slick.meta

import shapeless._

import com.github.nscala_time.time.Imports.DateTime

import tryp.slick.{Model, ModelTableI}

// trait SlickTypes
// {
//   import slick.lifted._
//   import slick.profile._
//   import slick.driver._
//   import slick.dbio._

//   type AnyAction[A] = DBIOAction[A, NoStream, Effect.All]
//   type DbAction = AnyAction[_]
//   type AssocIds = (ObjectId, ObjectId)
//   type ModelT = tryp.app.ModelT
//   type ModelTable[A <: Model] = ModelTableI[A] {
//     type TableElementType = A
//   }
//   type AnyModelTable = ModelTableI[_] {
//     type TableElementType <: Model
//   }
//   type ModelTableQuery[A <: Model] = TableQuery[_ <: ModelTable[A]]
//   type Dao[A <: Model] = tryp.slick.TrypQuery[A, _ <: ModelTable[A]]
//     with ModelTableQuery[A]
// }

// trait ShapelessExt
// {
//   type MNil = ObjectId :: DateTime :: DateTime :: HNil
//   def MNil = ObjectId.generate :: DateTime.now :: DateTime.now :: HNil
// }

// object SlickTypes
// extends SlickTypes

// trait GlobalsBase
// extends tryp.app.meta.GlobalsBase

// trait Globals
// extends tryp.app.meta.Globals
// with SlickTypes
// with TypedType
// with ModelQueryExt
// with ShapelessExt
// with ActionExt
