package deltadak.ui.taskcell.subtasks

import deltadak.HomeworkTask
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView

/**
 * Handles creation of subtasks.
 */
class SubtasksCreator(
        /** The TreeView in which the subtasks are. */
        val treeView: TreeView<HomeworkTask>) {

    /**
     * Creates an empty subtask, a child, of the parentItem.
     * @param parentItem The item to create a subtask in/under.
     */
    fun create(parentItem: TreeItem<HomeworkTask>) {
        // The item needs to be manually expanded.
        parentItem.value.expanded = true

        // Add a new subtask.
        val emptyItem = TreeItem<HomeworkTask>(HomeworkTask())
        parentItem.children.add(emptyItem)

        // Select the new subtask.
        treeView.selectionModel.select(emptyItem)

        // Get the index of the new subtask
        val index = treeView.selectionModel.selectedIndex

        // Layout the TreeView again, otherwise we can't directly edit an item.
        treeView.layout()

        // Get the new TreeItem from the selected index, pointing to emptyItem may not work.
        val newItem = treeView.getTreeItem(index)

        // Edit.
        treeView.edit(newItem)
    }

}