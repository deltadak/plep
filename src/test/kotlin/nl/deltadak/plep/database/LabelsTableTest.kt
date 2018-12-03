package nl.deltadak.plep.database

import nl.deltadak.plep.database.tables.Labels
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.deleteWhere
import org.jetbrains.exposed.sql.select
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import kotlin.test.assertEquals

object LabelsTableTest : Spek({
    given("database with labels table") {
        regularTransaction {
            SchemaUtils.drop(Labels)
            SchemaUtils.create(Labels)
        }

        on("inserting a value") {
            val label = "2WA30"
            Labels.insert(0, label)
            it("should be in the database, i.e., we can request it") {
                val requested: String = regularTransaction {
                    Labels.select { Labels.id eq 0 }.toList().map { it[Labels.label] }.first()
                }
                assertEquals(label, requested)
            }
        }

        on("updating a value") {
            val newLabel = "2WA40"
            Labels.updateOrInsert(0, newLabel)
            it("should be updated in the database") {
                val requested: String = regularTransaction {
                    Labels.select { Labels.id eq 0 }.toList().map { it[Labels.label] }.first()
                }
                assertEquals(newLabel, requested)
            }
        }

        on("'updating' a label that currently is not in the database") {
            val newLabel = "2MMD20"
            Labels.updateOrInsert(1, newLabel)
            it("should insert the label") {
                val requested: String = regularTransaction {
                    Labels.select { Labels.id eq 1 }.toList().map { it[Labels.label] }.first()
                }
                assertEquals(newLabel, requested)
            }
        }

        on("deleting a value") {
            regularTransaction { Labels.deleteWhere { Labels.id eq 0 } }
            it("should not be in the database, i.e., the query for its id should be empty") {
                val emptyQuery = regularTransaction {
                    Labels.select{ Labels.id eq 0 }.empty()
                }
                assertEquals(true, emptyQuery)
            }
        }
    }
})