package nl.deltadak.plep.database.tables

import org.jetbrains.exposed.sql.Table

object Tasks : Table() {
    val id = integer("id").primaryKey().uniqueIndex()
    val done = bool("done")
    val day = date("day")
    val task = varchar("task", length = 255)
//    val label = reference("label", Labels.label)
    val label = varchar("label", length = 10)
    val color = integer("color")
    val expanded = bool("expanded")
    val orderInDay = integer("orderInday")
}