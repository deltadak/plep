package deltadak.ui.gridpane

import deltadak.Database
import deltadak.HomeworkTask
import deltadak.database.DatabaseSettings
import deltadak.ui.Controller
import deltadak.ui.taskcell.TaskCell
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import javafx.scene.layout.AnchorPane
import java.time.LocalDate

/**
 * Initializes the whole UI of the GridPane.
 */
class GridPaneInitializer(
        /** The main Controller. */
        val controller: Controller) {

    /**
     * Sets up TreeViews for each day, including the editing of items and more.
     *
     * @param numberOfDays The total number of days to setup the gridpane with.
     * @param focusDate This date is the central date in the sense it is shown as the second day, which is by default today.
     */
    fun setup(numberOfDays: Int, focusDate: LocalDate) {

        // Anchor the main GridPane to the bottom of the Toolbar.
        AnchorPane.setTopAnchor(controller.gridPane, controller.toolBar.prefHeight)

        // First clear the GridPane, especially of the day text.
        controller.gridPane.children.clear()

        // Find out whether the number of columns should be calculated automatically or is user overridden.
        val isAutoColumns: Boolean = Database.INSTANCE.getSetting(DatabaseSettings.MAX_COLUMNS_AUTO.settingsName).toBoolean()
        val maxColumns = if (isAutoColumns) {
            Math.ceil(Math.sqrt(numberOfDays.toDouble())).toInt()
        } else {
            Database.INSTANCE.getSetting(DatabaseSettings.MAX_COLUMNS.settingsName).toInt()
        }

        // For every day, add a list for that day.
        for (index in 0 until numberOfDays) {

            // The day to add, starting from the day before the focusDate.
            val date = focusDate.plusDays(index.toLong() - 1 )

            // Initialize the TreeView.
            val rootItem = TreeItem<HomeworkTask>(HomeworkTask())
            rootItem.isExpanded = true
            val tree = TreeView<HomeworkTask>(rootItem)

            tree.isEditable = true
            tree.setCellFactory {
                val treeCell = TaskCell(controller, tree.root)
                treeCell.setup(tree, date)
                treeCell
            }
            tree.isShowRoot = false

            // Add a title to the list, which shows the day.


        }



    }

}