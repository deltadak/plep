package deltadak.database

import deltadak.Database
import deltadak.HomeworkTask
import javafx.concurrent.Task
import javafx.scene.control.ProgressIndicator
import java.time.LocalDate

/**
 * This class provides a facade to the database. General methods like pushing and pulling data from the database are included here, so extras can be added like proper user feedback and multithreading.
 */
class DatabaseFacade(
        /** For user feedback. */
        val progressIndicator: ProgressIndicator) {

    /**
     * Updates database using the given homework tasks for a day. Uses the progress indicator for user feedback.
     *
     * @param day Date from which the tasks are.
     * @param homeworkTasks Tasks to be put in the database.
     */
    fun pushData(day: LocalDate, homeworkTasks: List<List<HomeworkTask>> ) {

        val task: Task<Any> = object : Task<Any>() {
            @Throws(Exception::class)
            /** Specifies task. */
            public override fun call() {
                Database.INSTANCE.updateTasksDay(day, homeworkTasks)
            }
        }

        // Only switch it on and off if it's not yet on.
        if (!progressIndicator.isVisible) {

            // Switch on progress indicator.
            progressIndicator.isVisible = true

            // Switch off progress indicator.
            task.setOnSucceeded { progressIndicator.isVisible = false }

        }

        // Database calls will be executed on a different thread.
        executeMultithreading(task)

    }

    /**
     * Update only the parent tasks in the database.
     *
     * @param day The day which contains the tasks.
     * @param parentTasks The list with parents to update.
     */
    fun pushParentData(day: LocalDate, parentTasks: List<HomeworkTask>) {

        val task: Task<Any> = object : Task<Any>() {
            @Throws(Exception::class)
            /** Specifies task. */
            public override fun call() {
                Database.INSTANCE.updateParentsDay(day, parentTasks)
            }
        }

        // Only switch it on and off if it's not yet on.
        if (!progressIndicator.isVisible) {

            // Switch on progress indicator.
            progressIndicator.isVisible = true

            // Switch off progress indicator.
            task.setOnSucceeded { progressIndicator.isVisible = false }

        }

        // Database calls will be executed on a different thread.
        executeMultithreading(task)

    }

}