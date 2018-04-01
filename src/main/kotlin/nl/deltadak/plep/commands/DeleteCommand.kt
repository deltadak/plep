package nl.deltadak.plep.commands

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import nl.deltadak.plep.Database
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.ui.treeview.TreeViewCleaner
import java.time.LocalDate

/**
 * Command to delete a [HomeworkTask] from a TreeView.
 */
open class DeleteCommand(
        /** To show progress. */
        val progressIndicator: ProgressIndicator,
        /** Day of the task to delete. */
        val day: LocalDate,
        /** All the tasks of that day. */
        treeViewItemsImmutable: List<List<HomeworkTask>>,
        /** Index of the task in treeViewItems. */
        private val index: Int,
        /** To provide feedback immediately in the TreeView. */
        val tree: TreeView<HomeworkTask>) : Command() {

    /** List including subtasks. */
    private var deletedItemsList: List<HomeworkTask> = emptyList()

    /** All the tasks of the day, making the given list mutable in order to edit, original reference need not be preserved. */
    var treeViewItems: MutableList<MutableList<HomeworkTask>> = treeViewItemsImmutable.toMutableList().map{it.toMutableList()}.toMutableList()

    override fun executionHook() {

        if (treeViewItems.isEmpty()) {
            throw IllegalStateException("cannot delete item from empty treeview")
        }

        // Save the deleted tasks so we can undo later.
        deletedItemsList = treeViewItems[this.index]
        treeViewItems.removeAt(this.index)

        // Known to be null when testing.
        if (tree.root != null) {
            // Use the TreeView to delete an item, to provide user feedback.
            tree.root.children.removeAt(this.index)

            Database.INSTANCE.deleteByID(deletedItemsList[0].databaseID)
            TreeViewCleaner().cleanSingleTreeView(tree)
        }

    }

    override fun undoHook() {
        treeViewItems.add(this.index, deletedItemsList.toMutableList())

        if (tree.root != null) {
            // First add parent.
            val parent = TreeItem<HomeworkTask>(deletedItemsList[0])
            tree.root.children.add(this.index, parent)

            // Then the children.
            deletedItemsList.subList(1, deletedItemsList.size)
                    .forEach {
                        parent.children.add(TreeItem<HomeworkTask>(it))
                    }

            DatabaseFacade(progressIndicator).pushData(day, treeViewItems)
            TreeViewCleaner().cleanSingleTreeView(tree)
        }

    }

}