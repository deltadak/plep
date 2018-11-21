package nl.deltadak.plep.database

import nl.deltadak.plep.ui.treeview.TreeViewCleaner
import nl.deltadak.plep.ui.treeview.getAllTreeViews
import nl.deltadak.plep.ui.util.converters.getParentTasks
import nl.deltadak.plep.ui.util.converters.toObservableList
import javafx.beans.value.ObservableValue
import javafx.collections.ObservableList
import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import javafx.scene.layout.GridPane
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import nl.deltadak.plep.Database
import nl.deltadak.plep.HomeworkTask
import java.time.LocalDate

/**
 * Provides the content of the GridPane: all the tasks and subtasks.
 */
class ContentProvider {

    /**
     * Refreshes all treeviews using data from the database.
     *
     * @param gridPane The GridPane to fill with content.
     * @param focusDay The day which the second TreeView should belong to.
     * @param progressIndicator To show user feedback.
     */
    fun setForAllDays(gridPane: GridPane, focusDay: LocalDate, progressIndicator: ProgressIndicator) {

        val job = GlobalScope.launch {

                val treeViews = getAllTreeViews(gridPane)
                for (i in 0 until treeViews.size) {
                    val date = focusDay.plusDays(i .toLong() - 1)
                    setForOneDay(treeViews[i], date, progressIndicator)
                }
        }
    }

    /**
     * Requests tasks from database, and when done updates the treeview.
     *
     * @param tree TreeView to be updated.
     * @param localDate Day of that TreeView.
     * @param progressIndicator To show user feedback.
     */
    fun setForOneDay(tree: TreeView<HomeworkTask>, localDate: LocalDate, progressIndicator: ProgressIndicator) {

        /**
         * Contains database calls which take time.
         */
        fun databaseTask() {

            // Get all the tasks from the database.
            val allTasks = Database.INSTANCE.getTasksDay(localDate)

            // Find the parent tasks.
            val parents = allTasks.getParentTasks().toObservableList()

            // Clear current items.
            tree.root.children.clear()

            // Add the items from the database.
            for (i in 0 until parents.size) {
                setupParent(i, parents, tree, localDate, allTasks)
            }

        }

        progressIndicator.isVisible = true

        // Do the database stuff in a coroutine.
        val job = GlobalScope.launch { databaseTask() }

        job.invokeOnCompletion {
            // Make sure that there are empty tasks added to fill up.
            TreeViewCleaner().cleanSingleTreeView(tree)
            progressIndicator.isVisible = false
        }
    }


    /**
     * For each parent, add it to the TreeView together with its children.
     */
    @Suppress("UNUSED_ANONYMOUS_PARAMETER")
    private fun setupParent(parentIndex: Int, parents: ObservableList<HomeworkTask>, tree: TreeView<HomeworkTask>, localDate: LocalDate, allTasks: List<List<HomeworkTask>>) {
        // Add the parent task.
        val parent = TreeItem<HomeworkTask>(parents[parentIndex])
        tree.root.children.add(parent)
        parent.isExpanded = parent.value.expanded

        // Update database with whether item is expanded or not.
        parent.expandedProperty().addListener { observable: ObservableValue<out Boolean>, oldValue: Boolean, newValue: Boolean ->
            val task = parent.value
            task.expanded = newValue
            Database.INSTANCE.insertOrUpdateTask(localDate, task, parentIndex)
        }

        // Find out the family size corresponding to this parent.
        val familySize = allTasks[parentIndex].size
        // Add every subtask to the tree as a child of the parent.
        (1 until familySize)
                .map {
                    TreeItem<HomeworkTask>(allTasks[parentIndex][it])
                }
                .forEach {
                    tree.root.children[parentIndex].children.add(it)
                }

        // Insert an empty subtask at the end to allow the user to easily add more.
        if (familySize > 1) {
            tree.root.children[parentIndex].children.add(TreeItem<HomeworkTask>(HomeworkTask()))
        }
    }
}