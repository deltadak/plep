package nl.deltadak.plep.database.tables

import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.regularTransaction
import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.deleteWhere
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.select

object SubTasks : Table() {
    val parentID = integer("parentID")
    val done = bool("done")
    val task = varchar("task", length = 255)

    /**
     * Insert a subtask in the database if it doesn't exist yet. Otherwise do nothing.
     * Does not insert empty task.
     *
     * @param subtask to be inserted.
     * @param parentID id of its parent.
     */
    fun insert(subtask: HomeworkTask, parentID: Int) = regularTransaction {
        if (subtask.text != "" && parentID != -1 && !get(parentID).contains(subtask)) {
            insert {
                it[SubTasks.parentID] = parentID
                it[SubTasks.done] = subtask.done
                it[SubTasks.task] = subtask.text
            }
        }
    }

    /**
     * Get all subtasks that are children of a given parent task.
     *
     * @param parentID id of the parent task.
     */
    fun get(parentID: Int): List<HomeworkTask> = regularTransaction {
        select { SubTasks.parentID eq parentID }.map {
            HomeworkTask(done = it[SubTasks.done], text = it[SubTasks.task])
        }
    }

    /**
     * Delete all subtasks of a task.
     *
     * @param parentID id of the parent task.
     */
    fun delete(parentID: Int) = regularTransaction {
        deleteWhere { SubTasks.parentID eq parentID }
    }
}