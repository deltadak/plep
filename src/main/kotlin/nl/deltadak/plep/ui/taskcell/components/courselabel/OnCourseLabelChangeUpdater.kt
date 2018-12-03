package nl.deltadak.plep.ui.taskcell.components.courselabel

import javafx.application.Platform
import javafx.beans.InvalidationListener
import javafx.beans.Observable
import javafx.beans.value.ObservableValue
import javafx.scene.control.ComboBox
import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeView
import javafx.scene.control.cell.TextFieldTreeCell
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.InvalidationListenerWithBlocker
import nl.deltadak.plep.ui.util.converters.toHomeworkTaskList
import java.time.LocalDate

/**
 * Defines what happens when a combobox (the 'courselabel') selection is updated.
 */
@Suppress("UNUSED_ANONYMOUS_PARAMETER")
class OnCourseLabelChangeUpdater(
        /** For user feedback. */
        val progressIndicator: ProgressIndicator,
        /** The ComboBox on which to listen for changes. */
        private val comboBox: ComboBox<String>) {

    /**
     * Add a listener to the ComboBox which listens for value changes and applies them normally with one exception: when <no label> is selected then the empty string will be shown.
     *
     * @param treeCell The TreeCell in which the ComboBox resides, needed to updateOrInsert the courselabel of the task.
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
                task.label = newValue ?: ""
            }

        })

    }

    /**
     * Set listener on the ComboBox to updateOrInsert the database when the
     * selected index changes.
     *
     * @param tree TreeView which the LabelCell is in, needed for updating the database
     * @param day LocalDate which we need for updating the database
     *
     * @return The listener with blocker, so the block can be set when needed.
     */
    fun addDatabaseUpdaterChangeListener(tree: TreeView<HomeworkTask>, day: LocalDate) : InvalidationListenerWithBlocker {
        // Define a standard listener.
        val invalidationListener = InvalidationListener { observable: Observable ->
            DatabaseFacade(progressIndicator).pushData(day, tree.toHomeworkTaskList())
            // We do not need to cleanup here, as no tasks were added or deleted.
        }

        // Pass the invalidationlistener on to our custom listener.
        val labelChangeListener = InvalidationListenerWithBlocker(invalidationListener)

        // Add the listener which updates the database to the combobox.
        comboBox.selectionModel.selectedIndexProperty().addListener(labelChangeListener)

        return labelChangeListener

    }

}