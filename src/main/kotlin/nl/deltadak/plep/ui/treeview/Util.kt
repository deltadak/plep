package nl.deltadak.plep.ui.treeview

import javafx.scene.control.TreeView
import javafx.scene.layout.GridPane
import javafx.scene.layout.Pane
import javafx.scene.layout.VBox
import javafx.stage.Screen
import nl.deltadak.plep.HomeworkTask
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
                        .forEach { treeView ->
                            treeViews.add(treeView)
                        }
            }

    return treeViews

}