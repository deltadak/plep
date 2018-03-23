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
import java.lang.Integer.min
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
        taskCell.setOnDragExited {
            event -> setDragDone(event)
            event.consume()
            // Clean up immediately for a smooth reaction.
            TreeViewCleaner().cleanSingleTreeView(tree)
        }
    }

    private fun setDragDone(event: DragEvent) {
        if (event.transferMode == TransferMode.MOVE) {
            val dragBoard = event.dragboard
            val oldHomeworkTask = dragBoard.getContent(DATA_FORMAT) as HomeworkTask

            // The old item needs to be removed, but it could be that the item was moved up in the list, so the index increases by one, or (down or to another day), when the index doesn't change.

            // In the case that the current index is larger equal to the size of the list, then todo
            if (taskCell.index >= tree.root.children.size) {
                // happens...
            }
            val currentItem = tree.root.children[taskCell.index]
            val currentText = currentItem.value.text

            // If item was moved to an other day, or down in same list
            if (currentText == oldHomeworkTask.text) {
                // Set empty HomeworkTask.
                currentItem.value = HomeworkTask()
                taskCell.graphic = null
            } else {
                // The item was moved up in the same TreeView.
                val index = taskCell.index + 1
                tree.getTreeItem(index).value = HomeworkTask()
            }

            // Update database.
            // We only have to update the parents, because the subtasks only depend on their parents, and are independent of the day and the order in the day.
            val parentTasks = tree.toHomeworkTaskList().getParentTasks()
            DatabaseFacade(progressIndicator).pushParentData(day, parentTasks)
        }
    }
}