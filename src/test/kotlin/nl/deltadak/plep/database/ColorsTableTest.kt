package nl.deltadak.plep.database

import nl.deltadak.plep.database.tables.Colors
import nl.deltadak.plep.ui.util.DEFAULT_COLORS
import nl.deltadak.plep.ui.util.converters.toColor
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import kotlin.test.assertEquals

object ColorsTableTest: Spek({
    given("database with colors table") {
        regularTransaction {
            SchemaUtils.drop(Colors) // Drop the old table to ensure testing with fresh settings.
            SchemaUtils.create(Colors)
        }

        on("inserting default values") {
            (1..DEFAULT_COLORS.size).forEach { Colors.insert(it, DEFAULT_COLORS[it-1]) }

            it("should contain exactly five values") {
                assertEquals(5, Colors.getAll().size)
            }

            it("should contain the default values") {
                assertEquals(DEFAULT_COLORS.toSet(), Colors.getAll().toSet())
            }
        }

        on("requesting the hex value of a color") {
            val value = Colors.get(1)
            it("should equal the value we stored in the database") {
                assertEquals(DEFAULT_COLORS[0], value)
            }

            it("should be convertible to Color") {
                assertEquals(DEFAULT_COLORS[0].toColor(), value.toColor())
            }
        }

        on("editing a color") {
            val newColor = "f444a7"
            Colors.update(1, newColor)
            it("should update the setting, i.e., the value in the database should equal the new value") {
                assertEquals(newColor, Colors.get(1))
            }
        }
    }
})