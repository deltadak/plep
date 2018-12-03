package nl.deltadak.plep.database

import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.tables.SubTasks
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import kotlin.test.assertEquals
import kotlin.test.assertTrue

object SubTasksTableTest : Spek({
    given("database with a subtasks table") {
        regularTransaction {
            SchemaUtils.drop(SubTasks)
            SchemaUtils.create(SubTasks)
        }

        val task = HomeworkTask(done = false, text = "task")

        on("inserting a subtask") {
            val parentID = 1
            SubTasks.insert(task, parentID)
            it("should insert the task, i.e., the list of tasks with the same parentID "
                    + "should contain the just inserted task") {
                assertTrue {
                    regularTransaction {
                        SubTasks.get(parentID).contains(task)
                    }
                }
            }
        }

        on("inserting a subtask that is already in the database") {
            val sizeBefore = SubTasks.get(1).size
            SubTasks.insert(task, 1)
            it("should do nothing") {
                assertEquals(sizeBefore, SubTasks.get(1).size)
            }
        }

        on("trying to insert an empty subtask") {
            val sizeBefore = SubTasks.get(1).size
            SubTasks.insert(HomeworkTask(), 1)
            it("should do nothing") {
                assertEquals(sizeBefore, SubTasks.get(1).size)
            }
        }

        on("deleting the subtask of a task") {
            SubTasks.delete(1)
            it("should remove all subtasks of this parent, i.e., the set of subtasks "
                    + "with this parentID is empty") {
                assertTrue { SubTasks.get(1).isEmpty() }
            }
        }
    }
})