package nl.deltadak.plep.ui.draganddrop

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeCell
import javafx.scene.control.TreeView
import javafx.scene.input.DragEvent
import javafx.scene.input.TransferMode
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.ui.treeview.TreeViewCleaner
import nl.deltadak.plep.ui.util.converters.getParentTasks
import nl.deltadak.plep.ui.util.converters.toHomeworkTaskList
import java.time.LocalDate

/**
 * When the dragged object is dropped somewhere else, remove it.
 *
 * @property taskCell TreeCell which is notified when the task that was located here is dropped somewhere else.
 * @property tree TreeView to style as focused.
 * @property day of the TreeView.
 * @property progressIndicator Needed for refreshing UI.
 */
class DragDone(
        private val taskCell: TreeCell<HomeworkTask>,
        val tree: TreeView<HomeworkTask>,
        val day: LocalDate,
        val progressIndicator: ProgressIndicator) {

    init {
        taskCell.setOnDragDone {
            event -> setDragDone(event)
            event.consume()
            // Clean up immediately for a smooth reaction.
            TreeViewCleaner().cleanSingleTreeView(tree)
        }
    }

    private fun setDragDone(event: DragEvent) {
        if (event.transferMode == TransferMode.MOVE) {

            // Set empty HomeworkTask.
            taskCell.treeItem.value = HomeworkTask()
            taskCell.graphic = null

            // Update database.
            // We only have to updateOrInsert the parents, because the subtasks only depend on their parents, and are independent of the day and the order in the day.
            val parentTasks = tree.toHomeworkTaskList().getParentTasks()
            DatabaseFacade(progressIndicator).pushParentData(day, parentTasks)
        }
    }
}