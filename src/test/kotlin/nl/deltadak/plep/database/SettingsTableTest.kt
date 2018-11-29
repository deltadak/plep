package nl.deltadak.plep.database

import nl.deltadak.plep.database.namesanddefaults.DatabaseSettings
import nl.deltadak.plep.database.tables.Settings
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import kotlin.test.assertEquals

object SettingsTableTest : Spek({
    given("database with settings table") {
        SchemaUtils.create(Settings) // Drop the old table to ensure testing with fresh settings.
        SchemaUtils.drop(Settings)

        on("inserting default values") {
            DatabaseSettings.values().forEach {
                Settings.insert(it, it.default) }
            it("should contain exactly four values") {
                assertEquals(4, regularTransaction { Settings.selectAll().toList().size })
            }

            it("should contain the default values") {
                val rows = regularTransaction { Settings.selectAll().toList() }
                rows.forEach { row ->
                    val setting = DatabaseSettings.valueOf(row[Settings.name].toUpperCase())
                    assertEquals(setting.default, row[Settings.value])
                }
            }
        }

        on("requesting the value of a setting") {
            val value = Settings.get(DatabaseSettings.MAX_COLUMNS)
            it("should equal the value that we stored in the database") {
                assertEquals("3", value)
            }
        }

        on("editing a setting") {
            val name = DatabaseSettings.NUMBER_OF_MOVING_DAYS
            val newValue = "4"
            Settings.edit(name, newValue)
            it("should update the setting, i.e., the value in the database should equal the new value") {
                assertEquals(newValue, Settings.get(name))
            }
        }

        on("inserting a setting") {
            val name = DatabaseSettings.MAX_COLUMNS
            Settings.insert(name, "4")
            it("should do nothing if this setting is already in the database") {
                assertEquals("3", Settings.get(name))
            }
        }
    }

})