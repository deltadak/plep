package nl.deltadak.plep.ui.draganddrop

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeCell
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import javafx.scene.input.DragEvent
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.ContentProvider
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.ui.taskcell.selection.Selector
import nl.deltadak.plep.ui.treeview.TreeViewCleaner
import nl.deltadak.plep.ui.util.converters.getParentTasks
import nl.deltadak.plep.ui.util.converters.toHomeworkTaskList
import java.lang.Math.min
import java.time.LocalDate


/**
 * When the dragged object is dropped, update TreeView and database.
 *
 * @property taskCell TreeCell on which the task is dropped.
 * @property tree TreeView to update.
 * @property day of the TreeView.
 * @property progressIndicator Needed for refreshing UI.
 */
class DragDrop(
        private val taskCell: TreeCell<HomeworkTask>,
        val tree: TreeView<HomeworkTask>,
        val day: LocalDate,
        val progressIndicator: ProgressIndicator) {

    init {
        taskCell.setOnDragDropped { event ->
            setDragDrop(event)
        }
    }

    private fun setDragDrop(event: DragEvent) {
        val dragBoard = event.dragboard
        var success = false

        if (dragBoard.hasContent(DATA_FORMAT)) {
            val newHomeworkTask = dragBoard.getContent(DATA_FORMAT) as HomeworkTask
            success = dropTask(newHomeworkTask)
        }

        event.isDropCompleted = success
        event.consume()
        // Clean up immediately for a smooth reaction.
        TreeViewCleaner().cleanSingleTreeView(tree)

        // In order to show the subtasks again for the dropped item, we request them from the database.
        // This may seem slow but in practice fast enough.
        ContentProvider().setForOneDay(tree, day, progressIndicator)
    }

    private fun dropTask(newHomeworkTask: HomeworkTask): Boolean {

        // Insert a new task, removing the old one will happen in onDragDone.
        // The item could be dropped way below the existing list, hence the minimum to add it to the end.
        val index = min(taskCell.index, tree.root.children.size - 1)

        // If the task was dropped on an empty object, replace it, otherwise add it.
        val item = tree.root.children[index]
        if (item.value.text == "") {
            // Replace empty item.
            item.value = newHomeworkTask
        } else {
            val newItem = TreeItem<HomeworkTask>(newHomeworkTask)
            tree.root.children.add(index, newItem)
        }

        // Update database, only update the parents, because the subtasks only depend on their parents, and are independent of the day and the order in the day.
        DatabaseFacade(progressIndicator).pushParentData(
                day, tree.toHomeworkTaskList().getParentTasks()
        )

        // Clear selection on all other items immediately. This will result in a smooth reaction, whereas otherwise it takes a bit of noticable time before selection of the just-dragged item (on its previous location) is cleared.
        Selector(tree).deselectAll()

        return true
    }

}