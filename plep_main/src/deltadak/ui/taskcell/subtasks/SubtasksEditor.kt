package deltadak.ui.taskcell.subtasks

import deltadak.HomeworkTask
import deltadak.database.DatabaseFacade
import deltadak.ui.util.toHomeworkTaskList
import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeView
import java.time.LocalDate

/**
 * This class customizes what happens when users edit a subtask.
 */
class SubtasksEditor(
        /** For user feedback. */
        val progressIndicator: ProgressIndicator,
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
            DatabaseFacade(progressIndicator).pushData(localDate, tree.toHomeworkTaskList())

        }
    }

}