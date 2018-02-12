package deltadak.ui.treeview

import deltadak.HomeworkTask
import javafx.stage.Screen

/**
 * Calculates what the height of the TreeViews should be, depending on the screen size.
 *
 * @return Height.
 */
fun getTreeViewHeight(columns: Int, numberOfDays: Int): Int {
    val primaryScreenBounds = Screen.getPrimary().visualBounds
    val totalHeight = primaryScreenBounds.height.toInt()
    return totalHeight / (numberOfDays / columns)
}

/**
 * Calculates what the width of the TreeViews should be, depending on the screen size.
 *
 * @return Width.
 */
fun getTreeViewWidth(columns: Int): Int {
    val primaryScreenBounds = Screen.getPrimary().visualBounds
    val totalWidth = primaryScreenBounds.width.toInt()
    return totalWidth / columns
}

/**
 * Get all the parent tasks when given a list of lists of HomeworkTasks, assuming that the first item of a sublist is the parent task.
 *
 * @param homeworkFamilies The list of lists to get the parent tasks from.
 *
 * @return A list of HomeworkTasks, which are the parent tasks.
 */
fun List<List<HomeworkTask>>.getParentTasks(): List<HomeworkTask> = this.map { it[0] }
