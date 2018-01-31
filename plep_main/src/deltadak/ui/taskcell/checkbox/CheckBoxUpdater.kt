package deltadak.ui.taskcell.checkbox

import deltadak.HomeworkTask
import deltadak.database.DatabaseFacade
import deltadak.ui.AbstractController
import deltadak.ui.taskcell.ChangeListenerWithBlocker
import deltadak.ui.taskcell.TaskCell
import deltadak.ui.taskcell.selection.Selector
import deltadak.ui.taskcell.textlabel.TextLabelStyle
import deltadak.ui.util.STATIC.convertTreeToList
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import javafx.scene.control.TreeView
import javafx.scene.control.CheckBox
import java.time.LocalDate

@Suppress("UNUSED_ANONYMOUS_PARAMETER")
/**
 * Handles clicks on the checkbox.
 */
class CheckBoxUpdater(
        /** The main Controller. */
        val controller: AbstractController,
        /** The Checkbox to watch. */
        val checkbox: CheckBox) {

    /**
     * Sets a change listener on the CheckBox, to update the database on changes.
     * @param tree The TreeView the current TreeCell is in. We need this to update the database.
     * @param taskCell The TaskCell which contains this CheckBox. Not a general TreeCell because subitems like label and combobox are used.
     * @param localDate The date of the TreeView, and thus all the HomeworkTasks, in which the CheckBox is toggled.
     *
     * @return The change listener with blocker, so it can be blocker whenever needed.
     */
    fun setChangeListener(tree: TreeView<HomeworkTask>, taskCell: TaskCell, localDate: LocalDate) : ChangeListenerWithBlocker<Boolean> {

        val changeListener = ChangeListener<Boolean> { observable: ObservableValue<out Boolean>, oldValue: Boolean, newValue: Boolean ->

            // Update the Homeworktask in this cell.
            taskCell.treeItem.value.done = newValue

            // Set the right style on the text label.
            TextLabelStyle().setDoneStyle(newValue, taskCell.label, taskCell.comboBox)

            // Deselect the item, otherwise the selector changes color and overrides the item color.
            Selector(tree).deselectAll()

            checkIfAllSubtasksAreDone(taskCell, tree)

            DatabaseFacade(controller).updateDatabase(localDate, convertTreeToList(tree))

        }

        val doneChangeListener = ChangeListenerWithBlocker<Boolean>(changeListener)

        checkbox.selectedProperty().addListener(doneChangeListener)

        return doneChangeListener

    }

    /**
     * If the item of which the checkbox is toggled is a subtask, then we check if all subtasks are done.
     * If so, we mark its parent task as done.
     */
    private fun checkIfAllSubtasksAreDone(taskCell: TaskCell, tree: TreeView<HomeworkTask>) {

        if (taskCell.treeItem.parent != tree.root) {

            // Find the total number of subtasks for his parent.
            val totalSubtasks = taskCell.treeItem.parent.children.size

            // Find the number of tasks marked as done
            val doneSubtasks = taskCell.treeItem
                    .parent.children
                    .filter { it.value.done }
                    .map { 1 }
                    .sum()

            // If all the tasks are done, we mark the parent task as done.
            // This is a bit complicated because we always provide one more empty subtask to be edited.
            // Which means that with more than one subtask, the total of done subtasks should be one less than the total.
            // Border case: when there is only one subtask, it needs to be checked for the parent to be checked, so the amount of done subtasks needs to be at least one.
            if (totalSubtasks == (doneSubtasks + 1) && doneSubtasks > 0) {
                // Note that only calling ...getparent().getValue().setDone(true) is not enough to trigger the event listener of the parent item.
                val parent = taskCell.treeItem.parent.value
                parent.done = true
                // First we set the value null and then set the HomeworkTask again, because otherwise the cell will not notice something has changed and the checkbox won't be updated.
                taskCell.treeItem.parent.value = null
                taskCell.treeItem.parent.value = parent
            }

        }

    }

}