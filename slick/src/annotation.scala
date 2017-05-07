package tryp.annotation

import reflect.macros.whitebox.Context
import annotation.StaticAnnotation

class SyncSlick
extends StaticAnnotation
{
  def macroTransform(annottees: Any*) = macro SyncSlickAnnotation.process
}

class SyncSlickAnnotation(val c: Context)
extends tryp.slick.SyncSlick
with SchemaAnnotationBase
{
  override val slickTransformer = SyncSlickTransformer
}

class MobileSlick
extends StaticAnnotation
{
  def macroTransform(annottees: Any*) = macro MobileSchemaAnnotation.process
}

class MobileSchemaAnnotation(val c: Context)
extends tryp.slick.MobileSlick
with SchemaAnnotationBase
{
  override val slickTransformer = MobileSlickTransformer
}
