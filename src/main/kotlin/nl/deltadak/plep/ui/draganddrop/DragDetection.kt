package nl.deltadak.plep.ui.draganddrop

import javafx.scene.control.TreeCell
import javafx.scene.control.TreeItem
import javafx.scene.input.ClipboardContent
import javafx.scene.input.TransferMode
import nl.deltadak.plep.HomeworkTask

/**
 * When the dragging is detected, we place the content of the LabelCell in the DragBoard.
 */
class DragDetection(
        /** TreeCell on which the drag should be detected. */
        private val taskCell: TreeCell<HomeworkTask>,
        /** Root of the TreeView. */
        val root: TreeItem<HomeworkTask>) {

    init {

        taskCell.setOnDragDetected {
            event -> setDragDetection()
            event.consume()
        }

    }

    /**
     * What to do when a drag even is detected.
     */
    private fun setDragDetection() {

        val isParentTask = taskCell.treeItem.parent == root
        val isEmpty = taskCell.treeItem.value.text == ""

        if (!isEmpty && isParentTask) {
            val dragBoard = taskCell.startDragAndDrop(TransferMode.MOVE)
            val content = ClipboardContent()
            content[DATA_FORMAT] = taskCell.treeItem.value
            dragBoard.setContent(content)
        }

    }

}