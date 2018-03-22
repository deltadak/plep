package nl.deltadak.plep.ui.draganddrop

import javafx.scene.control.TreeCell
import javafx.scene.input.DragEvent
import javafx.scene.input.TransferMode
import nl.deltadak.plep.HomeworkTask

/**
 * When something is draggend over a cell.
 */
class DragOver(
        /** TreeCell on which the drag should be detected. */
        private val taskCell: TreeCell<HomeworkTask>) {

    init {
        taskCell.setOnDragOver {
            event -> setDragOver(event)
            event.consume()
        }
    }

    private fun setDragOver(event: DragEvent) {
        if (event.gestureSource != taskCell && event.dragboard.hasContent(DATA_FORMAT)) {
            event.acceptTransferModes(TransferMode.MOVE)
        }
    }

}