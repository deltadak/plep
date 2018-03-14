package nl.deltadak.plep.ui.settingspane.labelslist

import nl.deltadak.plep.ui.settingspane.LABELS_LIST_EXTRA_HEIGHT
import nl.deltadak.plep.ui.settingspane.LABELS_LIST_ROW_HEIGHT
import nl.deltadak.plep.ui.settingspane.LABELS_LIST_WIDTH
import nl.deltadak.plep.ui.settingspane.MAX_NUMBER_LABELS
import javafx.collections.FXCollections
import javafx.scene.control.ListView
import javafx.scene.control.cell.TextFieldListCell
import javafx.scene.layout.GridPane
import nl.deltadak.plep.Database
import kotlin.reflect.KMutableProperty

/**
 * The list with course labels can be edited by the user.
 */
class EditLabelsList {

    /**
     * Constructs a new ListView and sets it up for editing, sets location, etc.
     *
     * @param refreshUI Should refresh UI when called.
     */
    fun getNew(labelsListProperty: KMutableProperty<ListView<String>>, refreshUI: () -> Unit): ListView<String> {

        val labelsList = labelsListProperty.getter.call()

        // Set up the listview with empty labels to allow editing.
        val items = FXCollections.observableArrayList<String>("", "", "", "", "")

        // Restore last known labels, if any.
        val labels = Database.INSTANCE.labels
        for (i in 0 until labels.size) {
            items[i] = labels[i]
        }

        // Make cells editable, only setEditable(true) is not enough.
        labelsList.cellFactory = TextFieldListCell.forListView()
        labelsList.isEditable = true

        // Give the listview an FXML id to toggle visibility.
        labelsList.id = "labelsListView"

        labelsList.items = items
        labelsList.isVisible = false

        // Some magik layout constants.
        labelsList.prefWidth = LABELS_LIST_WIDTH
        labelsList.prefHeight = LABELS_LIST_ROW_HEIGHT * MAX_NUMBER_LABELS + LABELS_LIST_EXTRA_HEIGHT

        // Position the list in the settingspane.
        GridPane.setColumnIndex(labelsList, 1)
        GridPane.setRowIndex(labelsList, 0)
        GridPane.setRowSpan(labelsList, 2)

        // When editing a label in the list, update the database and refresh UI.
        labelsList.setOnEditCommit {
            event ->
            labelsList.items[event.index] = event.newValue
            // Also update the reference to the labels.
            labelsListProperty.setter.call(labelsList)
            Database.INSTANCE.updateLabel(event.index, event.newValue)
            refreshUI()
        }

        return labelsList

    }
}
