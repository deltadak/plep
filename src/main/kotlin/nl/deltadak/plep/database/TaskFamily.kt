package nl.deltadak.plep.database

import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.tables.SubTasks
import nl.deltadak.plep.database.tables.Tasks
import java.time.LocalDate

/**
 * Implements operations for the database on families of tasks, thus operations that affect at least one parent task
 * and one subtask.
 */
object TaskFamily {

    /**
     * Get all the tasks on a given day. Parent- and subtasks.
     *
     * @param day for which to get all the parent tasks.
     *
     * @return Lists with in each list parent tasks as first, child elements as
     * other elements.
     */
    fun getAllOnDay(day: LocalDate): List<List<HomeworkTask>> = regularTransaction {
        Tasks.getParentsOnDay(day).map { listOf(it).plus(SubTasks.get(it.databaseID)) }
    }

    /**
     * Delete a task and its subtasks.
     *
     * @param id of the task to be deleted.
     */
    fun deleteAll(id: Int) = regularTransaction {
        Tasks.delete(id)
        SubTasks.delete(id)
    }

    /**
     * Delete all parent and subtasks for a given day.
     *
     * @param day to delete all tasks from.
     */
    fun deleteAllDay(day: LocalDate) = regularTransaction {
        val parents = Tasks.getParentsOnDay(day)
        parents.forEach { deleteAll(it.databaseID) }
    }

    /**
     * Update a day in the database. Updates or insert the parent tasks, then for each
     * parent task deletes its current subtasks and inserts the 'new' ones.
     *
     * @param day to be updated.
     * @param homeworkTasks the new homework tasks.
     */
    fun updateAllDay(day: LocalDate, homeworkTasks: List<List<HomeworkTask>>) = regularTransaction {
        homeworkTasks.forEach {
            val parent = it.first()
            Tasks.insertUpdate(day, parent, homeworkTasks.indexOf(it))
            SubTasks.delete(parent.databaseID)
            it.forEach { sub -> if (sub.databaseID == -1) SubTasks.insert(sub, parent.databaseID) }
        }
    }

    /**
     * Copy a homework task and its subtasks to a new day.
     *
     * @param day to copy the tasks to.
     * @param task to be copied, this is the parent task.
     */
    fun copyAndInsert(day: LocalDate, task: HomeworkTask) = regularTransaction {
        val subtasks = SubTasks.get(task.databaseID)
        task.databaseID = Tasks.highestID() // Give the to be copied task a new id.
        Tasks.insertUpdate(day, task, Tasks.highestOrder(day))
        subtasks.forEach { SubTasks.insert(it, task.databaseID) }
    }
}