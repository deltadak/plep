package nl.deltadak.plep.ui.taskcell.selection

import javafx.beans.value.ObservableValue
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import nl.deltadak.plep.HomeworkTask

/**
 * Clears up selection in all other places when a TaskCell is selected.
 */
@Suppress("UNUSED_ANONYMOUS_PARAMETER")
class SelectionCleaner(
        /** a TreeView */
        val tree: TreeView<HomeworkTask>) {

    /**
     * If a TaskCell is selected, clear up all other selections as well.
     */
    fun addSelectionListener() {
        tree.selectionModel.selectedItemProperty().addListener({
            observable: ObservableValue<out TreeItem<HomeworkTask>>?, oldValue: TreeItem<HomeworkTask>?, newValue: TreeItem<HomeworkTask>? ->
            if (newValue != null) {
                Selector(tree).select({ tree.selectionModel.select(newValue) })
            }
        })
    }

}