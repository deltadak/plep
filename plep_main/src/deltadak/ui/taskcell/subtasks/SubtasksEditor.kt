package deltadak.ui.taskcell.subtasks

import deltadak.HomeworkTask
import deltadak.database.DatabaseFacade
import deltadak.ui.AbstractController
import deltadak.ui.util.TreeToListConverter
import javafx.scene.control.TreeView
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
                }
            }

            // Update the database.
            DatabaseFacade(controller).updateDatabase(localDate, TreeToListConverter().convertTreeToList(tree))

        }
    }

}