package nl.deltadak.plep.database.tables

import org.jetbrains.exposed.sql.Table

object Colors : Table() {
    val id = integer("id").uniqueIndex()
    val hex = varchar("hex", length = 10)

//    override fun create() = regularTransaction { SchemaUtils.create(Colors) }

//    override fun getAll(): List<ResultRow> = regularTransaction { Colors.selectAll().toList() }
}