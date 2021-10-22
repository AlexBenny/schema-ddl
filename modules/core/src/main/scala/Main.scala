import com.snowplowanalytics.iglu.schemaddl.jsonschema.Schema

import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.ObjectProperty._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.NumberProperty._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.ArrayProperty._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.StringProperty._
import com.snowplowanalytics.iglu.schemaddl.jsonschema.properties.CommonProperties._

object Main extends App {

  implicit def optconv[A](a: A): Option[A] = Some(a)

  val schema = Schema(
    `type` = Type.Object,
    properties = Properties(Map(
      "ipAddress" -> Schema(
        `type` = Type.String,
        format = Format.Ipv4Format
      ),
      "email" -> Schema(
        `type` = Type.String,
        format = Format.EmailFormat
      ),
      "someString" -> Schema(
        `type` = Type.String
      ),
      "netstedObject" -> Schema(
        `type` = Type.Object,
        properties = Properties(Map(
          "nestedKey" -> Schema(
            `type` = Type.Null
          ),
          "deeply" -> Schema(
            `type` = Type.Object,
            properties = Properties(Map(
              "blueDeep" -> Schema(
                `type` = Type.Integer,
                minimum =  Minimum.IntegerMinimum(0),
                maximum = Maximum.IntegerMaximum(32767)
              ),
              "deepUnionTypeArray" -> Schema(
                `type` = Type.Array,
                items = Items.ListItems(Schema(
                  `type` = Type.Union(Set(Type.Object, Type.String, Type.Null, Type.Integer)),
                  properties = Properties(Map(
                    "foo" -> Schema(
                      `type` = Type.String
                    )
                  )),
                  additionalProperties = AdditionalProperties.AdditionalPropertiesAllowed(false),
                  minimum = Minimum.IntegerMinimum(0),
                  maximum = Maximum.IntegerMaximum(32767)
                ))
              )
            )),
            additionalProperties = AdditionalProperties.AdditionalPropertiesAllowed(false)
          )
        )),
        additionalProperties = AdditionalProperties.AdditionalPropertiesAllowed(false)
      ),
      "emptyObject" -> Schema(
        `type` = Type.Object,
        properties = Properties(Map()),
        additionalProperties = AdditionalProperties.AdditionalPropertiesAllowed(false)
      ),
      "simpleArray" -> Schema(
        `type` = Type.Array,
        items = Items.ListItems(Schema(
          `type` = Type.Integer,
          minimum = Minimum.IntegerMinimum(0),
          maximum = Maximum.IntegerMaximum(32767)
        ))
      )
    )),
    additionalProperties = AdditionalProperties.AdditionalPropertiesAllowed(false)
  )

  def generate(schema: String): String = {
    val content: play.twirl.api.Txt = txt.Example(schema)
    content.body
  }

  // val schema = TypeSchema.fromJson("sample/src/main/resources/Foo.json")

  println(generate("Hello World"))
}
