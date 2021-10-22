import java.io.PrintWriter

import com.snowplowanalytics.iglu.schemaddl.bigquery.Type.Record
import com.snowplowanalytics.iglu.schemaddl.bigquery.Field
import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema
import io.circe.literal._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.circe.implicits._

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
            "type": "string",
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
    "required": ["name", "id"],
    "additionalProperties": false
}
      """

  val schema = Schema.parse(json)
  val field = Field.build(className, schema.getOrElse(Schema.empty), false)

  val fields = field.fieldType match {
    case Record(fields) => fields
    case _ => List.empty
  }
  // println(fields.map(_.name + ": " + _.fieldType) .mkString(","))

  val result = txt.Example(className, "iglu:com.snowplowanalytics.mobile/screen_view/jsonschema/1-0-0", fields)

  println(result.body)
  new PrintWriter(className + ".java") { write(result.body); close() }
}
