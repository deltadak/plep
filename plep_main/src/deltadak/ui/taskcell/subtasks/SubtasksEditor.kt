package deltadak.ui.taskcell.subtasks

import deltadak.HomeworkTask
import deltadak.database.DatabaseFacade
import deltadak.ui.AbstractController
import deltadak.ui.util.STATIC.NumberOfTasksInList
import deltadak.ui.util.STATIC.convertTreeToList
import javafx.scene.control.TreeView
import java.lang.Integer.max
import java.time.LocalDate

/**
 * This class customizes what happens when users edit a subtask.
 */
class SubtasksEditor(
        /** The main controller */
        val controller: AbstractController,
        /** The TreeView in which the subtasks resides. */
        val tree: TreeView<HomeworkTask>,
        /** The day corresponding to the TreeView. */
        val localDate: LocalDate) {

    /**
     * Initialize what happens after the user edited a subtask.
     */
    fun setup() {
        tree.setOnEditCommit { event ->
            // If we are editing one of the subtasks ...
            if(tree.editingItem.parent != tree.root) {
                // If we're not adding an empty task, create another subtask.
                if(event.newValue.text != "") {
                    SubtasksCreator(tree).create(tree.editingItem.parent)
                    // Scroll the last added item into view, so that the adding of new subtasks by using enter does not break.
                    tree.scrollTo(max(0,tree.editingItem.parent.children.size - (NumberOfTasksInList - 1) ))
                }
            }

            // Update the database.
            DatabaseFacade(controller).updateDatabase(localDate, convertTreeToList(tree))

        }
    }

}