package deltadak.ui.taskcell.courselabel

import deltadak.HomeworkTask
import deltadak.database.DatabaseFacade
import deltadak.ui.AbstractController
import deltadak.ui.taskcell.InvalidationListenerWithBlocker
import deltadak.ui.util.STATIC.convertTreeToList
import javafx.application.Platform
import javafx.beans.Observable
import javafx.beans.value.ObservableValue
import javafx.scene.control.ComboBox
import javafx.scene.control.TreeView
import javafx.scene.control.cell.TextFieldTreeCell
import java.time.LocalDate

/**
 * Defines what happens when a combobox (the 'courselabel') selection is updated.
 */
@Suppress("UNUSED_ANONYMOUS_PARAMETER")
class OnChangeUpdater(
        /** The main Controller. */
        val controller: AbstractController,
        /** The ComboBox on which to listen for changes. */
        val comboBox: ComboBox<String>) {

    /**
     * Add a listener to the ComboBox which listens for value changes and applies them normally with one exception: when <no label> is selected then the empty string will be shown.
     *
     * @param treeCell The TreeCell in which the ComboBox resides, needed to update the courselabel of the task.
     */
    fun addValueChangeListener(treeCell: TextFieldTreeCell<HomeworkTask>) {

        comboBox.valueProperty().addListener( { observable: ObservableValue<out String>?, oldValue: String?, newValue: String? ->

            val task = treeCell.treeItem.value

            if (newValue != null && newValue == "<no label>") {
                task.label = ""
                // Delay removing the combobox text because we cannot change the contents of an ObservableList while a change is in progress.
                // In practice the delay is unnoticable.
                Platform.runLater({comboBox.value = ""})
            } else {
                task.label = newValue
            }

        })

    }

    /**
     * Set listener on the ComboBox to update the database when the
     * selected index changes.
     *
     * @param tree TreeView which the LabelCell is in, needed for updating the database
     * @param day LocalDate which we need for updating the database
     *
     * @return The listener with blocker, so the block can be set when needed.
     */
    fun addDatabaseUpdaterChangeListener(tree: TreeView<HomeworkTask>, day: LocalDate) : InvalidationListenerWithBlocker {
        // Define a standard listener.
        val invalidationListener = { observable: Observable ->
            DatabaseFacade(controller).updateDatabase(day, convertTreeToList(tree))
            // We do not need to cleanup here, as no tasks were added or deleted.
        }

        // Pass the invalidationlistener on to our custom listener.
        val labelChangeListener = InvalidationListenerWithBlocker(invalidationListener)

        // Add the listener which updates the database to the combobox.
        comboBox.selectionModel.selectedIndexProperty().addListener(labelChangeListener)

        return labelChangeListener

    }

}