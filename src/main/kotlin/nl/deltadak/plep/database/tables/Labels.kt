package nl.deltadak.plep.database.tables

import nl.deltadak.plep.database.regularTransaction
import org.jetbrains.exposed.sql.*

object Labels : Table() {
    val id = integer("id").uniqueIndex()
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
    fun update(id: Int, label: String) = regularTransaction {
        update({ Labels.id eq id }) { it[this.label] = label }
        deleteWhere { Labels.label eq "" } // TODO is this necessary?
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

    /**
     * Delete a label from the database.
     *
     * @param id of the label that has to be deleted.
     */
    fun delete(id: Int) = regularTransaction {
        deleteWhere { Labels.id eq id }
    }
}