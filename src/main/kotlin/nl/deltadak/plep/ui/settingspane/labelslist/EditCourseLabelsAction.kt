package nl.deltadak.plep.ui.settingspane.labelslist

import javafx.scene.control.Button
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane

/**
 * Handles actions regarding the Button to updateOrInsert course labels.
 */
class EditCourseLabelsAction(
        /** The FXML reference to the button. */
        private val editLabelsButton: Button) {

    /**
     * Toggles the visibility of the course labels.
     *
     * @param editLabelsPane The GridPane which contains the labels.
     * @param slidingPane The Pane which contains all settings objects.
     */
    fun set(editLabelsPane: GridPane, slidingPane: AnchorPane) {
        editLabelsButton.setOnAction {
            // Show or hide the listview which contains the course labels, and the button which allows for removing labels.
            toggleFXMLObjectVisibility(editLabelsPane, "labelsListView")
            toggleFXMLObjectVisibility(editLabelsPane, "removeLabelButton")

            // Also push down or up the other elements.
            toggleHeight("editDaysPane", slidingPane, editLabelsPane)
            toggleHeight("colorsPane", slidingPane, editLabelsPane)
        }
    }

    /**
     * Toggles the visibility of an object present in the given GridPane.
     *
     * @param gridPane The UI element to search in.
     * @param id FXML id of the object to be toggled.
     */
    private fun toggleFXMLObjectVisibility(gridPane: GridPane, id: String) {
        val node = gridPane.lookup("#$id")
        node.isVisible = !node.isVisible
    }

    /**
     * Moves the FXML object (in the AnchorPane) up or down as much as the height of the given GridPane, pushing down the objects below it.
     *
     * @param id FXML id of the object to be toggled.
     * @param slidingPane The pane which contains the objects.
     * @param gridPane The height to use for pushing down the object.
     */
    private fun toggleHeight(id: String, slidingPane: AnchorPane, gridPane: GridPane) {
        val node = slidingPane.lookup("#$id")
        if (node.translateY == 0.0) {
            node.translateY = gridPane.height
        } else {
            node.translateY = 0.0
        }
    }

}