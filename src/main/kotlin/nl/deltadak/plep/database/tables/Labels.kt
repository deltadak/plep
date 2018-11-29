package nl.deltadak.plep.database.tables

import org.jetbrains.exposed.sql.Table

object Labels : Table() {
    val id = integer("id").uniqueIndex()
    val label = varchar("label", length = 10)
}