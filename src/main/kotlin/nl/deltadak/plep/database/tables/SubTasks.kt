package nl.deltadak.plep.database.tables

import org.jetbrains.exposed.sql.Table

object SubTasks : Table() {
    val parentID = integer("parentID")
    val done = bool("done")
    val task = varchar("task", length = 255)
}