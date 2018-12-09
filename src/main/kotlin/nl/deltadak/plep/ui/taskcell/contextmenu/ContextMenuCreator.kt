package nl.deltadak.plep.ui.taskcell.contextmenu

import javafx.scene.control.*
import javafx.scene.layout.GridPane
import nl.deltadak.plep.database.DatabaseFacade
import nl.deltadak.plep.database.tables.Colors
import nl.deltadak.plep.ui.taskcell.TaskCell
import nl.deltadak.plep.ui.taskcell.subtasks.SubtasksCreator
import nl.deltadak.plep.ui.taskcell.util.setBackgroundColor
import nl.deltadak.plep.ui.util.LABEL_COLOR_CONTEXT_MENU_ITEMS
import nl.deltadak.plep.ui.util.converters.toHomeworkTaskList
import nl.deltadak.plep.ui.util.repeatTask
import java.time.LocalDate

/**
 * Every TaskCell has a context menu behind a right-click.
 */
class ContextMenuCreator(
        /** The main UI element. */
        val gridPane: GridPane,
        /** The current focused day. */
        private val focusDay: LocalDate,
        /** For user feedback. */
        val progressIndicator: ProgressIndicator,
        /** The TaskCell to set a context menu on. */
        private val taskCell: TaskCell,
        /** The day to which this TreeView (and thus TreeCell) belongs. */
        private val day: LocalDate) {

    /** The number of weeks to show in the menu for repeating tasks. */
    private val numberOfWeeksToRepeat = 8

    /**
     * Creates a context menu, which gives options to:
     *  - add a subtask
     *  - repeat a task for a certain number of weeks
     *  - change the colour of a task.
     *
     * @return The ContextMenu.
     */
    fun create(): ContextMenu {
        // Get the tree which contains the TaskCell.
        val tree = taskCell.tree

        // Create the context menu.
        val contextMenu = ContextMenu()

        // Add a menu item to add a subtask.
        val addSubTaskMenuItem = MenuItem("Add subtask")
        addSubTaskMenuItem.setOnAction { SubtasksCreator(tree).create(taskCell.treeItem) }
        contextMenu.items.add(addSubTaskMenuItem)

        // Add a menu item for repetition of tasks.
        val repeatTasksMenu: Menu = createRepeatMenu(gridPane, focusDay)
        contextMenu.items.add(repeatTasksMenu)

        // Add a horizontal line as separator.
        val separatorItem = SeparatorMenuItem()
        contextMenu.items.add(separatorItem)

        val colors = Colors.getAll()

        for (colorID in 0..4) {
            // Initialize the color menu items with a certain number of spaces.
            val colorItem = MenuItem(LABEL_COLOR_CONTEXT_MENU_ITEMS)
            colorItem.style = "-fx-background-color: #" + colors[colorID]

            // Add the on-click action.
            colorItem.setOnAction {
                taskCell.setBackgroundColor(colorID)
                taskCell.treeItem.value.colorID = colorID
                DatabaseFacade(progressIndicator).pushData(day, tree.toHomeworkTaskList())
            }

            contextMenu.items.add(colorItem)
        }

        return contextMenu

    }

    /**
     * Creates the Menu which provides an option to repeat a task weekly, for the next x weeks.
     *
     * @param gridPane Needed for repeatTask to refresh the UI.
     * @param focusDay Needed for repeatTask to refresh the UI.
     *
     * @return A drop down Menu.
     */
    private fun createRepeatMenu(gridPane: GridPane, focusDay: LocalDate): Menu {
        // Initialize the numbers as menu items.
        val repeatTasksMenu = Menu("Repeat for x weeks")
        for (i in 1..numberOfWeeksToRepeat) {
            val menuItem = MenuItem("$i")
            menuItem.setOnAction {
                repeatTask(gridPane, repeatNumber = i, task = taskCell.item, day = day, focusDay = focusDay, progressIndicator = progressIndicator)
            }
            repeatTasksMenu.items.add(menuItem)
        }

        return repeatTasksMenu

    }

}