package com.snowplowanalytics.iglu.schemaddl.trackers

trait ObjCProperty {
  def name: String
  def description: Option[String]
  def optional: Boolean
  def objCType: String
  def pointerType: String = objCType + " *"
  def isFirst: Boolean = false
}

case class StringObjCProperty(name: String,
                              description: Option[String],
                              optional: Boolean,
                              maxLength: Option[BigInt] = None,
                              minLength: Option[BigInt] = None,
                              pattern: Option[String] = None,
                              objCType: String = "NSString"
                             ) extends ObjCProperty

case class IntegerObjCProperty(name: String,
                               description: Option[String],
                               optional: Boolean,
                               multipleOf: Option[BigInt] = None,
                               minimum: Option[BigInt] = None,
                               maximum: Option[BigInt] = None,
                               objCType: String = "NSNumber"
                              ) extends ObjCProperty

case class NumberObjCProperty(name: String,
                              description: Option[String],
                              optional: Boolean,
                              multipleOf: Option[BigDecimal] = None,
                              minimum: Option[BigDecimal] = None,
                              maximum: Option[BigDecimal] = None,
                              objCType: String = "NSNumber"
                             ) extends ObjCProperty

case class BooleanObjCProperty(name: String,
                               description: Option[String],
                               optional: Boolean,
                               objCType: String = "NSNumber"
                              ) extends ObjCProperty

// TODO: to develop
case class ArrayObjCProperty(name: String,
                             description: Option[String],
                             optional: Boolean,
                             objCType: String = "NSList"
                            ) extends ObjCProperty

// TODO: to develop
case class ObjectObjCProperty(name: String,
                              description: Option[String],
                              optional: Boolean,
                              objCType: String = "NSObject"
                             ) extends ObjCProperty

object ObjCProperty {
  def build(property: Option[EnrichedProperty]): Option[ObjCProperty] = {
    if (property.isEmpty) {
      return None
    }
    val prop = property.get
    prop match {
      case p: BooleanEnrichedProperty => Option(BooleanObjCProperty(p.name, p.description, p.optional))
      case p: StringEnrichedProperty => Option(StringObjCProperty(p.name, p.description, p.optional, p.maxLength, p.minLength, p.pattern))
      case p: IntegerEnrichedProperty => Option(IntegerObjCProperty(p.name, p.description, p.optional, p.multipleOf, p.minimum, p.maximum))
      case p: NumberEnrichedProperty => Option(NumberObjCProperty(p.name, p.description, p.optional, p.multipleOf, p.minimum, p.maximum))
      case p: ArrayEnrichedProperty => Option(ArrayObjCProperty(p.name, p.description, p.optional))
      case p: ObjectEnrichedProperty => Option(ObjectObjCProperty(p.name, p.description, p.optional))
      case _ => None
    }
  }
}
