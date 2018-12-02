package nl.deltadak.plep.database.tables

import nl.deltadak.plep.database.regularTransaction
import org.jetbrains.exposed.sql.*

object Colors : Table() {
    val id = integer("id").uniqueIndex()
    val hex = varchar("hex", length = 10)

    /**
     * Insert a color in the database if its id is not in the database yet.
     *
     * @param id of the color.
     * @param hex string of the color.
     */
    fun insert(id: Int, hex: String) = regularTransaction {
        insertIgnore {
            it[this.id] = id
            it[this.hex] = hex
        }
    }

    /**
     * Edit color, i.e., update its hex value in the database.
     *
     * @param id of the color to be updated.
     * @param value the new value to be stored in the database.
     */
    fun update(id: Int, value: String) = regularTransaction {
        update({ Colors.id eq id + 1 }) { it[hex] = value }
    }

    /**
     * Request all the colors in the database.
     *
     * @return all the hex values that are in the database.
     */
    fun getAll(): List<String> = regularTransaction { selectAll().toList().map { it[Colors.hex] } }

    /**
     * Request the value of a color. Takes the first of all the colors with this id. Since the id is unique, this gives
     * the only result.
     *
     * @return the hex of the requested color.
     */
    fun get(id: Int): String = regularTransaction {
        select { Colors.id eq id + 1 }.map { it[hex] }.first()
    }
}