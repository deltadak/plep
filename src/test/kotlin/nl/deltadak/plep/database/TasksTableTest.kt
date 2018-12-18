package nl.deltadak.plep.database

import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.tables.Tasks
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.deleteAll
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import java.time.LocalDate
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * Test the methods from the [Tasks] table.
 */
object TasksTableTest : Spek({
    /** Delete all tasks. */
    fun deleteAll() = regularTransaction { Tasks.deleteAll() }
    given("database with tasks table") {
        regularTransaction {
            SchemaUtils.drop(Tasks)
            SchemaUtils.create(Tasks)
        }

        val date = LocalDate.now()
        val task = HomeworkTask(false, "task")

        on("inserting a nonempty task") {
            Tasks.insertUpdate(date, task, 1)
            it("should insert the value in the database") {
                assertTrue { Tasks.getParentsOnDay(date).contains(task) }
            }
            deleteAll()
        }

        on("inserting an empty task") {
            val sizeBefore = Tasks.getParentsOnDay(date).size
            Tasks.insertUpdate(date, HomeworkTask(), order = 1)
            it("should not insert anything") {
                assertEquals(sizeBefore, Tasks.getParentsOnDay(date).size)
            }
        }

        on("updating the tasks on a day") {
            Tasks.insertUpdate(date, task, order = 1)
            val tasks = Tasks.getParentsOnDay(date) + listOf(HomeworkTask(true, "new task 1"), HomeworkTask(false, "new task 2"))
            Tasks.updateDay(date, tasks)
            it("should update the tasks") {
                tasks.forEach { assertTrue { Tasks.getParentsOnDay(date).contains(it) } }
            }
            deleteAll()
        }

        on("requesting the highest order in a day") {
            Tasks.insertUpdate(date, task, 2)
            val high = Tasks.highestOrder(date)
            it("should equal the highest order + 1") {
                assertEquals(3, high)
            }
            deleteAll()
        }

        on("requesting the highest id") {
            task.databaseID = 1
            Tasks.insertUpdate(date, task, 1)
            val highID = Tasks.highestID()
            it("should equal the highest id + 1") {
                assertEquals(2, highID)
            }
        }

        on("deleting a task") {
            Tasks.delete(task.databaseID)
            it("should not be in the database anymore") {
                assertFalse { Tasks.getParentsOnDay(date).contains(task) }
            }
        }

        on("deleting tasks on a day") {
            Tasks.insertUpdate(date, task, 2)
            Tasks.deleteDay(date)
            it("should give an empty database") {
                assertEquals(0, Tasks.getParentsOnDay(date).size)
            }
        }
    }
})