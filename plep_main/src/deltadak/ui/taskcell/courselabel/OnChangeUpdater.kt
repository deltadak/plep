package deltadak.ui.taskcell.courselabel

import deltadak.HomeworkTask
import javafx.application.Platform
import javafx.beans.value.ObservableValue
import javafx.scene.control.ComboBox
import javafx.scene.control.cell.TextFieldTreeCell

/**
 * Update the text shown on the combo box where there are changes, with some customizations.
 */
@Suppress("UNUSED_ANONYMOUS_PARAMETER")
class OnChangeUpdater(
        /** The ComboBox on which to listen for changes. */
        val comboBox: ComboBox<String>,
        /** The TreeCell in which the ComboBox resides. */
        val treeCell: TextFieldTreeCell<HomeworkTask>) {

    /**
     * Add a listener to the ComboBox which listens for changes and applies them normally with one exception: when <no label> is selected then the empty string will be shown.
     */
    fun addChangeListener() {

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
}