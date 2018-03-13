package nl.deltadak.plep.keylisteners

import javafx.scene.input.KeyCode
import javafx.scene.layout.GridPane
import nl.deltadak.plep.commands.UndoFacility

/**
 * Listener for key presses bound to the 'undo' action.
 */
class UndoKeyListener {

    /**
     * Bind the 'undo' action to the CTRL+Z key action.
     *
     * @param gridPane A UI element to bind it to.
     * @param undoFacility Should provide the undo action.
     */
    fun set(gridPane: GridPane, undoFacility: UndoFacility) {
        gridPane.setOnKeyPressed {
            event ->
            if (event.isControlDown && event.code == KeyCode.Z) {
                undoFacility.undo()
            }
        }
    }

}