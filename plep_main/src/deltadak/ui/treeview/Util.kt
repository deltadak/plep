package deltadak.ui.treeview

import deltadak.HomeworkTask
import javafx.scene.control.TreeView
import javafx.scene.layout.GridPane
import javafx.scene.layout.Pane
import javafx.scene.layout.VBox
import javafx.stage.Screen
import java.util.ArrayList

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

/**
 * Find all TreeViews in the GridPane.
 *
 * @param gridPane To search in for TreeViews.
 *
 * @return All TreeViews that are direct children of the GridPane.
 */
fun getAllTreeViews(gridPane: GridPane): List<TreeView<HomeworkTask>> {

    val treeViews = ArrayList<TreeView<HomeworkTask>>()

    // The GridPane contains VBox contains label, pane and TreeView.
    gridPane.children.filter { it is VBox }
            .forEach {
                (it as Pane).children
                        .filterIsInstance<TreeView<HomeworkTask>>()
                        .forEach {
                            treeViews.add(it)
                        }
            }

    return treeViews

}