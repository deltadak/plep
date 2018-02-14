package deltadak.ui.util.STATIC

import deltadak.HomeworkTask
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView

/**
 * This file contains several useful converter methods, specified top level to be easily callable (like static methods).
 */

/**
 * Get all the parent tasks when given a list of lists of HomeworkTasks, assuming that the first item of a sublist is the parent task.
 *
 * @return A list of HomeworkTasks, which are the parent tasks.
 */
fun List<List<HomeworkTask>>.getParentTasks(): List<HomeworkTask> = this.map { it[0] }

/**
 * Converts a TreeView to a list of lists of tasks. The first item of each
 * list is the parent task, the items after that are its subtasks.
 *
 * @return List&lt;List&lt;HomeworkTask&gt;&gt;
 */
fun TreeView<HomeworkTask>.toHomeworkTaskList(): List<List<HomeworkTask>> {

    // Create a list with the tree items of the parent tasks.
    val parentItems = this.root.children

    // Create a list with homework tasks of the parent tasks.
    val parentTasks = parentItems.toFlatList()

    // Create the list to eventually return.
    val tasks = mutableListOf<List<HomeworkTask>>()

    for (i: Int in 0 until parentItems.size) {
        // Get the sub tree items of parent task i, and store them in a list.
        val childItems = parentItems[i].children

        // Store the subtasks of parent task i in a list.
        val childTasks = childItems.toFlatList()

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
fun <H> ObservableList<TreeItem<H>>.toFlatList() = this.map {it.value}

/**
 * Convert a List to an ObservableList.
 *
 * @return Converted list.
 */
fun <H> List<H>.toObservableList(): ObservableList<H> = FXCollections.observableList(this)
