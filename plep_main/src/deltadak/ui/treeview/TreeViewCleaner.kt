package deltadak.ui.treeview

import deltadak.HomeworkTask
import deltadak.ui.util.NUMBER_OF_TASKS_IN_LIST
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView

/**
 * This class supplies methods useful in cleaning up the UI of a TreeView. Can be handy when you don't know what changed, but you know something did.
 * Does not access the database or change content in any case, so should not be too slow.
 */
class TreeViewCleaner {

    /**
     * Removes empty items in the tree view, and then fills it up with empty items. This way there will not appear any gaps in the list of tasks.
     *
     * WARNING Try to avoid using this as much as possible, it's a brute force way of cleaning up.
     *
     * @param tree The TreeView to clean up.
     */
    fun cleanSingleTreeView(tree: TreeView<HomeworkTask>?) {

        if (tree != null) {

            // Remove empty items.
            tree.root.children.removeIf { it.value.text == "" }

            // Remove empty subitems.
            tree.root.children.forEach { parent ->
                if (parent.children != null) {
                    parent.children.removeIf { it.value.text == "" }
                }
            }

            // Add empty items.
            for (i in 0 until NUMBER_OF_TASKS_IN_LIST) {
                val parents = tree.root.children
                if (i >= parents.size) {
                    val newItem = TreeItem<HomeworkTask>(HomeworkTask())
                    parents.add(newItem)
                } else { // If there are still tasks, we haven't got to the end of the task list.
                    // Add empty subtask if needed.
                    if (parents[i].children != null && parents[i].children.size > 0) {
                        parents[i].children.add(TreeItem<HomeworkTask>(HomeworkTask()))
                    }
                }
            }
        }

    }

}