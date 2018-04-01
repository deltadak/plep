package nl.deltadak.plep.ui.settingspane.labelslist

import javafx.scene.control.Button
import javafx.scene.control.ListView
import nl.deltadak.plep.Database
import kotlin.reflect.KMutableProperty

/**
 * This buttons removes the selected course label from the list.
 */
class RemoveLabelAction(
        /** The FXML reference to the button. */
        private val removeLabelButton: Button) {

    /**
     * Set the button action.
     *
     * @param labelsListProp Should be a variable reference to the list of labels.
     * @param refreshUI Should refresh the UI when called.
     */
    fun set(labelsListProp: KMutableProperty<ListView<String>>,
            refreshUI: () -> Unit) {

        removeLabelButton.setOnAction {

            // Get current value.
            val labelsList = labelsListProp.getter.call()

            val selectedIndex = labelsList.selectionModel.selectedIndex
            // Removing an item means replacing it with an empty item, so it is again editable.
            labelsList.items[selectedIndex] = ""
            Database.INSTANCE.updateLabel(selectedIndex, "")
            refreshUI()

        }
    }
}
