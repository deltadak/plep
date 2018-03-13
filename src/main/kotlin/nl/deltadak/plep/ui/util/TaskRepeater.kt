package nl.deltadak.plep.ui.util

import nl.deltadak.plep.database.ContentProvider
import javafx.scene.control.ProgressIndicator
import javafx.scene.layout.GridPane
import nl.deltadak.plep.Database
import nl.deltadak.plep.HomeworkTask
import java.time.LocalDate

/**
 * Repeats a task for a number of weeks.
 * @param gridPane The main UI element.
 * @param repeatNumber The number of weeks to repeat the task.
 * @param homeworkTask The HomeworkTask to be repeated.
 * @param day The current day, to be able to calculate on what days to add the task.
 * @param focusDay Needed to refresh the GridPane after execution.
 * @param progressIndicator Needed to refresh the GridPane after execution.
 */
fun repeatTask(gridPane: GridPane, repeatNumber: Int, task: HomeworkTask, day: LocalDate, focusDay: LocalDate, progressIndicator: ProgressIndicator) {
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
    // Refresh the UI.
   ContentProvider().setForAllDays(gridPane, focusDay, progressIndicator)
}