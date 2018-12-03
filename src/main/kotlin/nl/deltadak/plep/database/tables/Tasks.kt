package nl.deltadak.plep.database.tables

import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.regularTransaction
import org.jetbrains.exposed.sql.Date
import org.jetbrains.exposed.sql.Table
import java.time.LocalDate

object Tasks : Table() {
    val id = integer("id").primaryKey().uniqueIndex()
    val done = bool("done")
    val day = date("day")
    val task = varchar("task", length = 255)
//    val label = reference("label", Labels.label)
    val label = varchar("label", length = 10)
    val color = integer("color")
    val expanded = bool("expanded")
    val orderInDay = integer("orderInday")


//    /**
//     * Get all the parent tasks on a given day.
//     *
//     * @param day for which to get all the parent tasks.
//     *
//     * @return List<HomeworkTask>
//     */
//    fun getParentsOnDay(day: LocalDate): List<HomeworkTask> = regularTransaction {  }
//
//    /**
//     * Get all the tasks on a given day. Parent- and subtasks.
//     *
//     * @param day for which to get all the parent tasks.
//     *
//     * @return Lists with in each list parent tasks as first, child elements as
//     * other elements.
//     */
//    fun getAllOnDay(day: LocalDate): List<List<HomeworkTask>> = regularTransaction {
//
//    }
//
//    /**
//     * Delete a task and its subtasks.
//     *
//     * @param id of the task to be deleted.
//     */
//    fun delete(id: Int) = regularTransaction {  }
//
//    /**
//     * Update a day in the database.
//     *
//     * @param day to be updated.
//     * @param homeworkTasks the new homeworktasks.
//     */
//    fun updateDay(day: LocalDate, homeworkTasks: List<List<HomeworkTask>>) = regularTransaction {
//
//    }
//
//    fun copyAndInsert() = regularTransaction {  }
}