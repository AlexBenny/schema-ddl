import java.io.PrintWriter

import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema
import io.circe.literal._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.circe.implicits._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.ObjectProperty
import com.snowplowanalytics.iglu.schemaddl.trackers.{EnrichedProperty, ObjCProperty}

case class MyProp(name: String, `type`: String, description: String, required: Boolean)

object Main extends App {

  val className = "ScreenView"
  val json =
   json"""
{
    "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
    "description": "Schema for a screen view event",
    "self": {
        "vendor": "com.snowplowanalytics.mobile",
        "name": "screen_view",
        "format": "jsonschema",
        "version": "1-0-0"
    },
    "type": "object",
    "properties": {
        "name": {
            "type": "string",
            "description": "The name of the screen viewed."
        },
        "type": {
            "type": "string",
            "description": "The type of screen that was viewed e.g feed / carousel."
        },
        "id": {
            "type": "string",
	        "format": "uuid",
            "maxLength": 36,
            "description": "An ID from the associated screenview event."
        },
        "previousName": {
            "type": ["string", "null"],
            "description": "The name of the previous screen."
        },
        "previousId": {
            "type": "string",
	        "format": "uuid",
            "description": "A screenview ID of the previous screenview."
        },
        "previousType": {
            "type": "string",
            "description": "The screen type of the previous screenview."
        },
        "transitionType": {
            "type": "string",
            "description": "The type of transition that led to the screen being viewed."
        }
    },
    "minProperties": 2,
    "required": ["name", "id", "previousName"],
    "additionalProperties": false
}
      """

  val schemaUnwrapped = Schema.parse(json)
  /*
  val field = Field.build(className, schema.getOrElse(Schema.empty), false)

  val fields = field.fieldType match {
    case Record(fields) => fields
    case _ => List.empty
  }
  val result = txt.Example(className, "iglu:com.snowplowanalytics.mobile/screen_view/jsonschema/1-0-0", fields)

  //println(result.body)
  new PrintWriter(className + ".java") { write(result.body); close() }
*/
  // --

  val schema = schemaUnwrapped.getOrElse(Schema.empty)
  println("\n\nSchema =\n" + schema)

  val requiredFields = schema.required match {
    case Some(value) => Option(value.value)
    case None => None
  }

  val properties = schema.properties.getOrElse(ObjectProperty.Properties(Map.empty)).value.toSeq
  val enrichedProperties = properties.map(item => {
    EnrichedProperty.build(item._1, item._2, required = requiredFields)
  })

  println("\n\nEnrichedProperties =\n" + enrichedProperties)

  val objcProperties = enrichedProperties.flatMap(ObjCProperty.build)

  println("\n\nObjCProperties =\n" + objcProperties)

  val result = txt.ObjCHeader("SP" + className.capitalize, className, "iglu:com.snowplowanalytics.mobile/screen_view/jsonschema/1-0-0", objcProperties)
  new PrintWriter("SP" + className.capitalize + ".h") { write(result.body); close() }
}
