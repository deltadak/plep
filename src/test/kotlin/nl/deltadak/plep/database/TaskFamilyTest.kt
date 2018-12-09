package nl.deltadak.plep.database

import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.tables.SubTasks
import nl.deltadak.plep.database.tables.Tasks
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import java.time.LocalDate
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

object TaskFamilyTest : Spek({
    given("database with tasks table") {
        regularTransaction {
            SchemaUtils.drop(Tasks)
            SchemaUtils.create(Tasks)
        }

        val today = LocalDate.now()
        val tomorrow = today.plusDays(1)

        val todaytasks = listOf(
                listOf(HomeworkTask(text = "today 1"),
                        HomeworkTask(text = "sub 1.1")),
                listOf(HomeworkTask(text = "today 2"),
                        HomeworkTask(text = "sub 2.1"))
        )
        val tomorrowTask = listOf(listOf(HomeworkTask(text = "tomorrow 1"), HomeworkTask(text = "tomsub 1.1")))

        on("inserting tasks in the database") {
            TaskFamily.updateAllDay(today, todaytasks)
            it("should store the new tasks in the database") {
                val tasks = TaskFamily.getAllOnDay(today)
                // The sets should be equal.
                todaytasks.flatten().forEach { assertTrue { tasks.flatten().contains(it) } }
                tasks.flatten().forEach { assertTrue { todaytasks.flatten().contains(it) } }
            }
        }

        on("requesting all the tasks of a day") {
            val tasks = TaskFamily.getAllOnDay(today)
            it("should return all the tasks of that day") {
                // The sets should be equal, order doesn't matter.
                todaytasks.flatten().forEach { assertTrue { tasks.flatten().contains(it) } }
                tasks.flatten().forEach { assertTrue { todaytasks.flatten().contains(it) } }
            }

            it("should not return tasks of any other day") {
                tomorrowTask.forEach { it.forEach { task -> assertFalse { tasks.flatten().contains(task) } } }
            }

            it("should return the parent tasks as first in the lists") {
                todaytasks.forEach { original -> assertTrue { tasks.map { it.first() }.contains(original.first()) } }
            }

        }

        on("copying and inserting a task to a new day") {
            val task = HomeworkTask(text = "today 1")
            TaskFamily.copyAndInsert(tomorrow, task)
            it("should insert the task on the new day") {
                assertTrue { Tasks.getParentsOnDay(tomorrow).contains(task) }
            }
        }

        on("deleting a task and its subtasks") {
            val taskToBeDeleted = Tasks.getParentsOnDay(today).first()
            TaskFamily.deleteAll(taskToBeDeleted.databaseID)
            it("should delete the task") {
            assertFalse { Tasks.getParentsOnDay(today).contains(taskToBeDeleted) }
            }

            it("should delete the subtasks") {
                assertFalse {
                    regularTransaction {
                        SubTasks.selectAll().map { it[SubTasks.parentID] }
                    }.contains(taskToBeDeleted.databaseID)
                }
            }
        }
    }

})