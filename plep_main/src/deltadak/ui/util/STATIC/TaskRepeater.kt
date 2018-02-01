package deltadak.ui.util.STATIC

import deltadak.Database
import deltadak.HomeworkTask
import deltadak.ui.Controller
import java.time.LocalDate

/**
 * Repeats a task for a number of weeks.
 * @param controller The main Controller.
 * @param repeatNumber The number of weeks to repeat the task.
 * @param homeworkTask The HomeworkTask to be repeated.
 * @param day The current day, to be able to calculate on what days to add the task.
 */
fun repeatTask(controller: Controller, repeatNumber: Int, task: HomeworkTask, day: LocalDate) {
    // Repeating 2 times means adding one to next week and week after.
    (1..repeatNumber)
            .map {
                // Find new day.
                day.plusWeeks(it.toLong())
            }
            .forEach {
                // Copy the task and its subtasks to the new day.
                Database.INSTANCE.copyAndInsertTask(it, task)
            }
    // Refresh the UI
    controller.refreshAllDays()
}