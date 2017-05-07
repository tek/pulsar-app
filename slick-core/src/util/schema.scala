package tryp.slick.meta

object SchemaImports
// extends tryp.slick.meta.SlickTypes
// with tryp.slick.meta.TypedType
// with tryp.slick.meta.ModelQueryExt
// with tryp.slick.meta.ShapelessExt
{
  type DateTime = com.github.nscala_time.time.Imports.DateTime
  val DateTime = com.github.nscala_time.time.Imports.DateTime
}
