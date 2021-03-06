package nl.deltadak.plep.ui.taskcell.subtasks

import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.ui.util.NUMBER_OF_TASKS_IN_LIST

/**
 * Handles creation of subtasks.
 */
class SubtasksCreator(
        /** The TreeView in which the subtasks are. */
        private val treeView: TreeView<HomeworkTask>) {

    /**
     * Creates an empty subtask, a child, of the parentItem.
     * @param parentItem The item to create a subtask in/under.
     */
    fun create(parentItem: TreeItem<HomeworkTask>) {
        // The item needs to be manually expanded.
        parentItem.value.expanded = true

        // Add a new subtask.
        var emptyItem = TreeItem<HomeworkTask>(HomeworkTask())

        // If there already is an empty item at the end ready to be edited, use that one.
        if (parentItem.children.size > 0 && parentItem.children.last().value.text == "") {
            emptyItem = parentItem.children.last()
        } else {
            parentItem.children.add(emptyItem)
        }

        // Select the new subtask.
        treeView.selectionModel.select(emptyItem)

        // In theory to updateOrInsert an item:
//        treeView.updateOrInsert(emptyItem)
        // May help:
//        treeView.layout()

        // Scroll the last added item into view, so that the adding of new subtasks by using enter does not break.
        treeView.scrollTo(Integer.max(0, emptyItem.parent.children.size - (NUMBER_OF_TASKS_IN_LIST - 1)))
    }

}