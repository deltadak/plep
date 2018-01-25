package deltadak.ui.util

import deltadak.HomeworkTask
import deltadak.ui.Controller
import javafx.collections.ObservableList
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView

/**
 * Converter between a TreeView and a list of lists of Homeworktasks.
 */
class TreeToListConverter {

    /**
     * Converts a TreeView to a list of lists of tasks. The first item of each
     * list is the parent task, the items after that are its subtasks.
     *
     * @param tree The TreeView to convert.
     *
     * @return List&lt;List&lt;HomeworkTask&gt;&gt;
     */
    fun convertTreeToList(tree: TreeView<HomeworkTask>) : List<List<HomeworkTask>> {

        // Create a list with the tree items of the parent tasks.
        val parentItems = tree.root.children

        // Create a list with homework tasks of the parent tasks.
        val parentTasks = convertTreeItemsToList(parentItems)

        // Create the list to eventually return.
        val tasks = mutableListOf<List<HomeworkTask>>()

        for (i: Int in 0 until parentItems.size) {
            // Get the sub tree items of parent task i, and store them in a list.
            val childItems = parentItems[i].children

            // Store the subtasks of parent task i in a list.
            val childTasks = convertTreeItemsToList(childItems)

            // Create a list containing one parent and its children.
            val oneFamily = mutableListOf<HomeworkTask>()

            // Add the parent to the family.
            oneFamily.add(parentTasks[i])

            // Add its children to the family.
            oneFamily.addAll(childTasks)

            // Add the family to the nested list of tasks
            tasks.add(oneFamily)

        }

        return tasks

    }

    /**
     * Converts List of TreeItems of Homeworktasks to List of Homeworktasks.
     *
     * @param list List to convert.
     *
     * @return List of Homeworktasks.
     */
    fun convertTreeItemsToList(list: ObservableList<TreeItem<HomeworkTask>>) = list.map{ it.value }

}