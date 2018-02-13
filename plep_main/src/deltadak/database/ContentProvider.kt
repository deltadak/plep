package deltadak.database

import deltadak.Database
import deltadak.HomeworkTask
import deltadak.ui.Controller
import deltadak.ui.treeview.TreeViewCleaner
import deltadak.ui.treeview.getParentTasks
import deltadak.ui.util.STATIC.toObservableList
import javafx.beans.value.ObservableValue
import javafx.collections.ObservableList
import javafx.concurrent.Task
import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import java.time.LocalDate

/**
 * Provides the content of the GridPane: all the tasks and subtasks.
 */
class ContentProvider {

    /**
     *
     */
    fun setForAllDays() {

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

        val task = object : Task<Any>() {
            @Throws(Exception::class)
            override fun call(): Boolean {
                databaseTask()
                return true
            }
        }

        task.setOnSucceeded {
            // Make sure that there are empty tasks added to fill up.
            TreeViewCleaner().cleanSingleTreeView(tree)
            progressIndicator.isVisible = false
        }

        executeMultithreading(task)

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