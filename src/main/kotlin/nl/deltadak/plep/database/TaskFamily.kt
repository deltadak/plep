package nl.deltadak.plep.database

import nl.deltadak.plep.HomeworkTask
import java.time.LocalDate

class TaskFamily {

    /**
     * Get all the tasks on a given day. Parent- and subtasks.
     *
     * @param day for which to get all the parent tasks.
     *
     * @return Lists with in each list parent tasks as first, child elements as
     * other elements.
     */
    fun getAllOnDay(day: LocalDate): List<List<HomeworkTask>> = regularTransaction { listOf(listOf(HomeworkTask())) }

    /**
     * Delete a task and its subtasks.
     *
     * @param id of the task to be deleted.
     */
    fun deleteAll(id: Int) = regularTransaction {  }

    /**
     * Update a day in the database. Tasks and SubTasks.
     *
     * @param day to be updated.
     * @param homeworkTasks the new homework tasks.
     */
    fun updateAllDay(day: LocalDate, homeworkTasks: List<List<HomeworkTask>>) = regularTransaction {  }

    /**
     * Copy a homework task and its subtasks to a new day.
     *
     * @param day to copy the tasks to.
     * @param task to be copied.
     */
    fun copyAndInsert(day: LocalDate, task: HomeworkTask) = regularTransaction {  }
}