package deltadak.ui.taskcell.selection

import deltadak.HomeworkTask
import javafx.scene.control.TreeView
import javafx.scene.layout.VBox

/**
 * Provides ability to select TaskCells programmatically.
 */
class Selector(
        /** TreeView in which to select items. */
        private val tree: TreeView<HomeworkTask>) {

    /**
     * Select a TreeItem in a TreeView, and deselect all other items.
     * @param selectionFunction The function to select the right item, can select using index or TreeItems. e.g. tree.selectionModel.select(index)
     */
    fun select(selectionFunction: () -> Unit) {
        // We want to clear the selection on all the other listviews, otherwise weird 'half-selected' greyed out cells are left behind.

        // A TreeView is inside a VBox inside the GridPane, so node will be a VBox
        // Hence we need to circumvent a little to clear selection of all other listviews
        tree.parent.parent.childrenUnmodifiable
                .filterIsInstance<VBox>()
                .map { // We assume the title (a Label) is first, Pane is second, treeview is third
                    it.children[2]
                }
                .filterIsInstance<TreeView<*>>()
                .forEach { // First deselect all of them...
                    it.selectionModel.clearSelection()
                }

        // ... then reselect this one
        selectionFunction()
    }
}