package deltadak.ui.util.STATIC

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
