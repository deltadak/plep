package deltadak.ui.gridpane

import deltadak.Database
import deltadak.HomeworkTask
import deltadak.commands.UndoFacility
import deltadak.database.ContentProvider
import deltadak.database.DatabaseSettings
import deltadak.keylisteners.TaskDeletionInitialiser
import deltadak.ui.Controller
import deltadak.ui.taskcell.TaskCell
import deltadak.ui.treeview.getTreeViewHeight
import deltadak.ui.treeview.getTreeViewWidth
import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane
import java.time.LocalDate
import kotlin.reflect.KMutableProperty

/**
 * Initializes the whole UI of the GridPane.
 */
class GridPaneInitializer(
        /** The main Controller. */
        val controller: Controller,
        /** Facility which provides deletion. */
        val undoFacility: UndoFacility,
        /** User feedback. */
        val progressIndicator: ProgressIndicator) {

    /**
     * IMPORTANT Pass variables which are not constants by reference, with ::varName in Kotlin or use JavaHelper in Java.
     */
    fun setup(gridPane: GridPane, numberOfDays: KMutableProperty<Int>, focusDate: KMutableProperty<LocalDate>, toolBarHeight: Double) {
        setup(gridPane, numberOfDays.getter.call(), focusDate.getter.call(), toolBarHeight)
    }

    /**
     * Sets up TreeViews for each day, including the editing of items and more.
     *
     * @param gridPane The GridPane to setup.
     * @param numberOfDays The total number of days to setup the gridpane with.
     * @param focusDate This date is the central date in the sense it is shown as the second day, which is by default today.
     */
    @Deprecated("Use other setup method")
    fun setup(gridPane: GridPane, numberOfDays: Int, focusDate: LocalDate, toolBarHeight: Double) {

        // Anchor the main GridPane to the bottom of the Toolbar.
        AnchorPane.setTopAnchor(gridPane, toolBarHeight) //toolBar.prefHeight

        // First clear the GridPane, especially of the day text.
        gridPane.children.clear()

        // Find out whether the number of columns should be calculated automatically or is user overridden.
        val isAutoColumns: Boolean = Database.INSTANCE.getSetting(DatabaseSettings.MAX_COLUMNS_AUTO.settingsName).toBoolean()
        val nrColumns = if (isAutoColumns) {
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
                treeCell.setup(tree, date, progressIndicator, gridPane, focusDate)
                treeCell
            }
            tree.isShowRoot = false

            // Add the tree with title to the GridPane.
            TreeContainer(tree, date).addTreeToGridPane(gridPane, index, nrColumns)

            // Request the content to be set.
            ContentProvider().setForOneDay(tree, date, progressIndicator)

            // Setup deletion of tasks.
            TaskDeletionInitialiser(progressIndicator, undoFacility).addDeleteKeyListener(tree, localDate = date)

            tree.prefWidth = getTreeViewWidth(nrColumns).toDouble()
            tree.prefHeight = getTreeViewHeight(nrColumns, numberOfDays).toDouble()

        }



    }

}