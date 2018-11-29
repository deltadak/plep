package nl.deltadak.plep.database.tables

import nl.deltadak.plep.database.namesanddefaults.DatabaseSettings
import nl.deltadak.plep.database.regularTransaction
import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.insertIgnore
import org.jetbrains.exposed.sql.select
import org.jetbrains.exposed.sql.update

object Settings : Table() {
    val name = varchar("name", length = 50).primaryKey().uniqueIndex()
    val value = varchar("value", length = 50)

    /**
     * Insert a setting in the database if its name isn't in the database yet.
     *
     * @param name of the setting.
     * @param value of the setting.
     */
    fun insert(name: DatabaseSettings, value: String) = regularTransaction {
        insertIgnore {
            it[this.name] = name.settingsName
            it[this.value] = value
        }
    }

    /**
     * Edit setting, i.e., update its value in the database.
     *
     * @param name type of [DatabaseSettings] to be edited.
     * @param value to be submitted to the database.
     */
    fun edit(name: DatabaseSettings, value: String) = regularTransaction {
            update({ Settings.name eq name.settingsName }) { it[this.value] = value }
    }

    /**
     * Request the value of a setting. Gets all the rows from the database with the corresponding name, and takes the
     * first. This is valid because the names are unique, so there will always be only one result.
     *
     * @param name of the setting to be requested.
     */
    fun get(name: DatabaseSettings): String = regularTransaction {
        select { Settings.name eq name.settingsName }.distinct().map { it[value] }.first()
    }
}