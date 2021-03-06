package nl.deltadak.plep.ui.gridpane

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.commands.UndoFacility
import nl.deltadak.plep.database.ContentProvider
import nl.deltadak.plep.database.settingsdefaults.SettingsDefaults
import nl.deltadak.plep.database.tables.Settings
import nl.deltadak.plep.keylisteners.TaskDeletionInitialiser
import nl.deltadak.plep.ui.taskcell.TaskCell
import nl.deltadak.plep.ui.treeview.getTreeViewHeight
import nl.deltadak.plep.ui.treeview.getTreeViewWidth
import java.time.LocalDate
import kotlin.reflect.KMutableProperty

/**
 * Initializes the whole UI of the GridPane.
 */
class GridPaneInitializer(
        /** Facility which provides deletion. */
        private val undoFacility: UndoFacility,
        /** User feedback. */
        val progressIndicator: ProgressIndicator) {

    /**
     * Sets up TreeViews for each day, including the editing of items and more.
     *
     * @param gridPane The GridPane to setup.
     * @param numberOfDaysProperty The total number of days to setup the gridpane with. Pass reference instead of value, with ::numberOfDays.
     * @param focusDateProperty This date is the central date in the sense it is shown as the second day, which is by default today. Pass reference instead of value, with ::focusDate.
     */
    fun setup(gridPane: GridPane, numberOfDaysProperty: KMutableProperty<Int>, focusDateProperty: KMutableProperty<LocalDate>, toolBarHeight: Double) {

        val numberOfDays = numberOfDaysProperty.getter.call()
        val focusDate = focusDateProperty.getter.call()

        // Anchor the main GridPane to the bottom of the Toolbar.
        AnchorPane.setTopAnchor(gridPane, toolBarHeight) //toolBar.prefHeight

        // First clear the GridPane, especially of the day text.
        gridPane.children.clear()

        // Find out whether the number of columns should be calculated automatically or is user overridden.
        val isAutoColumns: Boolean = Settings.get(SettingsDefaults.MAX_COLUMNS_AUTO).toBoolean()
        val nrColumns = if (isAutoColumns) {
            Math.ceil(Math.sqrt(numberOfDays.toDouble())).toInt()
        } else {
            Settings.get(SettingsDefaults.MAX_COLUMNS).toInt()
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
                TaskCell(tree, date).apply { setup(progressIndicator, gridPane, focusDate) }
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