package nl.deltadak.plep.commands

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.ui.treeview.TreeViewCleaner
import java.time.LocalDate

/**
 * Delete a subtask only.
 */
class DeleteSubtaskCommand(progressIndicator: ProgressIndicator, day: LocalDate, treeViewItemsImmutable: List<List<HomeworkTask>>, index: Int, tree: TreeView<HomeworkTask>) : DeleteCommand(progressIndicator, day, treeViewItemsImmutable, index, tree) {

    private val deletedTask: HomeworkTask = treeViewItems.flatten()[index]

    /** Index of parent in treeview, only counting parents */
    private var parentIndex: Int = -1
    /** Index of subtask in the list of children of it's parent */
    private var indexWithinParent: Int = -1

    override fun executionHook() {
        if (treeViewItems.isEmpty()) {
            throw IllegalStateException("cannot delete item from empty treeview")
        }

        // Remove task from saved state.
        for (i in treeViewItems.indices) {
            val taskList = treeViewItems[i].toMutableList()
            if (taskList.contains(deletedTask)) {
                parentIndex = i
                // Subtract one because the parent is the first item in the list.
                indexWithinParent = taskList.indexOf(deletedTask) - 1
                taskList.remove(deletedTask)
            }
        }

        val parent = tree.root.children[parentIndex]
        parent.children.removeAt(indexWithinParent)

        DatabaseFacade(progressIndicator).pushData(day, treeViewItems)
        TreeViewCleaner().cleanSingleTreeView(tree)

    }

    override fun undoHook() {

        if (parentIndex == -1 || indexWithinParent == -1) {
            throw IllegalStateException("cannot find the task to re-add")
        }

        // We add one to the index because the first one in the list is the parent task, and we count from there.
        treeViewItems[parentIndex].add(indexWithinParent + 1, deletedTask)

        val parent = tree.root.children[parentIndex]
        parent.children.add(indexWithinParent, TreeItem(deletedTask))

        DatabaseFacade(progressIndicator).pushData(day, treeViewItems)
        TreeViewCleaner().cleanSingleTreeView(tree)

    }

}