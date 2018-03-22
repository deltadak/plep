package nl.deltadak.plep.ui.draganddrop

import javafx.scene.control.TreeCell
import javafx.scene.control.TreeView
import javafx.scene.input.DragEvent
import nl.deltadak.plep.HomeworkTask

/**
 * When the dragged object exists, restyle to normal.
 *
 * @property taskCell TreeCell on which the drag should be detected.
 * @property tree TreeView to style as focused.
 */
class DragExit(
        private val taskCell: TreeCell<HomeworkTask>,
        val tree: TreeView<HomeworkTask>) {

    init {
        taskCell.setOnDragExited {
            event -> setDragExit(event)
            event.consume()
        }
    }

    private fun setDragExit(event: DragEvent) {
        if (event.gestureSource != taskCell && event.dragboard.hasContent(DATA_FORMAT)) {
            tree.style = "-fx-background-color: -fx-base;"
        }
    }

}
