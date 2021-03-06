package nl.deltadak.plep.database.tables

import nl.deltadak.plep.database.regularTransaction
import org.jetbrains.exposed.sql.*

/**
 * Describes the Labels table for the database, and implements operation on this table.
 */
object Labels : Table() {
    /** ID of the label. */
    val id = integer("id").uniqueIndex()
    /** Text of the label. */
    val label = varchar("label", length = 10)

    /**
     * Request all the labels that are stored in the database, and return
     * them as Strings in an ArrayList.
     *
     * @return labels
     */
    fun getAll(): List<String> = regularTransaction {
        selectAll().map { it[Labels.label] }
    }

    /**
     * Update label in the database.
     *
     * @param id of the label to be updated.
     * @param label new value of the label.
     */
    fun updateOrInsert(id: Int, label: String) = regularTransaction {
        deleteWhere { Labels.id eq id }
        insert(id, label)
        deleteWhere { Labels.label eq "" }
    }

    /**
     * Insert a new label in the database.
     *
     * @param id of the to be inserted label.
     * @param label (text) of the to be inserted label.
     */
    fun insert(id: Int, label: String) = regularTransaction {
        insert {
            it[Labels.id] = id
            it[Labels.label] = label
        }
    }
}