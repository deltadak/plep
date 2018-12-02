package nl.deltadak.plep.ui.taskcell.subtasks

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeView
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.ui.util.converters.toHomeworkTaskList
import java.time.LocalDate

/**
 * This class customizes what happens when users update a subtask.
 */
class SubtasksEditor(
        /** For user feedback. */
        val progressIndicator: ProgressIndicator,
        /** The TreeView in which the subtasks resides. */
        val tree: TreeView<HomeworkTask>,
        /** The day corresponding to the TreeView. */
        private val localDate: LocalDate) {

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