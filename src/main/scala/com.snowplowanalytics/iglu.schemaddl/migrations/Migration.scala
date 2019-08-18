/*
 * Copyright (c) 2014-2016 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package com.snowplowanalytics.iglu.schemaddl.migrations

// cats
import cats.data._
import cats.implicits._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer}

import com.snowplowanalytics.iglu.schemaddl.{MigrationMap, OrderedSubSchemas}
import com.snowplowanalytics.iglu.schemaddl.migrations.SchemaList._

/**
 * Class representing common information about Schema change, without details
 * about specific DDLs
 *
 * @param vendor Schema vendor
 * @param name Schema name
 * @param from source Schema version
 * @param to target Schema version
 * @param diff ordered map of added Schema properties
 */
case class Migration(vendor: String, name: String, from: SchemaVer.Full, to: SchemaVer.Full, diff: SchemaDiff) {
  override def toString: String = s"Migration of $vendor/$name from ${from.asString} to ${to.asString} with $diff"
}

object Migration {

  /** Represents error cases which can be get from `MigrateFrom` function */
  sealed trait MigrateFromError extends Product with Serializable

  case object MigrateFromError {

    /** Returned when current schema is not found in the given schemas */
    case object SchemaKeyNotFoundInSchemas extends MigrateFromError

    /** Returned when current schema is last version of given schemas */
    case object SchemaInLatestState extends MigrateFromError

    /** Returned when something goes wrong while creating SchemaListSegment */
    case object SegmentCreationError extends MigrateFromError
  }

  /**
   * Build migration from a `sourceSchema` to the last schema in list of `successiveSchemas`
   * This method requires all intermediate schemas because we need to keep an order of properties
   */
  def buildMigration(source: SchemaListSegment): Migration = {
    val base = source.schemas.head.self.schemaKey
    val diff = SchemaDiff.build(source)
    Migration(base.vendor, base.name, base.version, source.schemas.last.self.schemaKey.version, diff)
  }

  /**
   * Build [[MigrationMap]], a map of source Schema to it's migrations,
   * where all source Schemas belong to a single model-revision Schema criterion
   *
   * @param schemaListFulls List of SchemaListFull to create migrations
   * @return map of each Schema to list of all available migrations
   */
  def buildMigrationMap(schemaListFulls: NonEmptyList[SchemaListFull]): MigrationMap = {
    // groupBy of NonEmptyList requires cats.Order of SchemaMap
    // however there is no dependency like this in groupBy of Scala List.
    // Therefore, it is converted to Scala List initially and reconverted
    // to NonEmptyList afterward.
    buildMigrationMatrix(schemaListFulls)
      .toList.map(source => (source.schemas.head.self, buildMigration(source)))
      .groupBy(_._1)
      .mapValues(m => NonEmptyList.fromListUnsafe(m.map(_._2)))
  }

  /**
    * Build a map of source Schema to its OrderedSubSchemas, where all source Schemas
    * are last version of their model group
    * @param orderedSchemasList source Schemas
    * @return map of last version of Schema model group to its OrderedSubSchemas
    */
  def buildOrderedSubSchemasMap(orderedSchemasList: NonEmptyList[SchemaList]): Map[SchemaMap, OrderedSubSchemas] =
    orderedSchemasList.map {
      case s: SingleSchema => s.schema.self -> FlatSchema.buildOrderedSubSchemas(s)
      case s: SchemaListFull => s.schemas.last.self -> FlatSchema.buildOrderedSubSchemas(s)
    }.toList.toMap

  /**
    * Get a migration from current state to the latest known schema
    * where error can be if schema key does not belong to these schemas
    * or schema key is already a latest state
    * @param current schemaKey of current state
    * @param schemas schemas of model group which ordered according to
    *                their version
    * @return return Either.left in case of error cases which is specified
    *         above or Migration as Either.right
    */
  def migrateFrom(current: SchemaKey, schemas: SchemaListFull): Either[MigrateFromError, Migration] =
    schemas.schemas.dropWhile_(_.self.schemaKey != current) match {
      case Nil => MigrateFromError.SchemaKeyNotFoundInSchemas.asLeft
      case _ :: Nil => MigrateFromError.SchemaInLatestState.asLeft
      case h :: tail =>
        val i = schemas.schemas.toList.map(_.self.schemaKey).indexOf(current)
        schemas.afterIndex(i)
          .toRight(MigrateFromError.SegmentCreationError)
          .map(buildMigration)
  }

  /**
    * Build schema list segments from ordered schemas in order to use
    * them while creating migrations
    * @param schemaListFulls list of SchemaListFull to create segments from them
    * @return list of segments which are extracted from given ordered schemas
    */
  private[migrations] def buildMigrationMatrix(schemaListFulls: NonEmptyList[SchemaListFull]): NonEmptyList[SchemaListSegment] =
    schemaListFulls.flatMap(_.extractSegments)
}
