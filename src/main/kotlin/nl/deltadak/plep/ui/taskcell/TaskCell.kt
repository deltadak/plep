package nl.deltadak.plep.ui.taskcell

import javafx.collections.FXCollections
import javafx.scene.control.*
import javafx.scene.control.cell.TextFieldTreeCell
import javafx.scene.layout.GridPane
import nl.deltadak.plep.Database
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.tables.Labels
import nl.deltadak.plep.ui.draganddrop.*
import nl.deltadak.plep.ui.taskcell.components.checkbox.CheckBoxUpdater
import nl.deltadak.plep.ui.taskcell.components.courselabel.OnCourseLabelChangeUpdater
import nl.deltadak.plep.ui.taskcell.contextmenu.ContextMenuCreator
import nl.deltadak.plep.ui.taskcell.selection.SelectionCleaner
import nl.deltadak.plep.ui.taskcell.subtasks.SubtasksEditor
import nl.deltadak.plep.ui.taskcell.treecell.TaskConverter
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.ChangeListenerWithBlocker
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.InvalidationListenerWithBlocker
import java.time.LocalDate

/**
 * Custom TextFieldTreeCell to hold tasks including labels, checkboxes and more.
 *
 * @property tree This is the TreeView this TaskCell is in.
 * @property day The date to which this TreeView (and thus TreeCell) belongs.
 */
class TaskCell(val tree: TreeView<HomeworkTask>, val day: LocalDate): TextFieldTreeCell<HomeworkTask>() {

    /** The CheckBox of this TaskCell. */
    val checkBox = CheckBox()
    /** The Label of this TaskCell. */
    val label = Label("")
    /** The ComboBox of this TaskCell. */
    val comboBox = ComboBox<String>(FXCollections.observableArrayList(Labels.getAll()).apply{ add(0, "<no label>") })

    /** A reference to a listener which listens for changes of the ComboBox, but which can be disabled. */
    private lateinit var labelChangeListener: InvalidationListenerWithBlocker
    /** A reference to a listener which listens for changes of the CheckBox, but which can be disabled. */
    private lateinit var doneChangeListener: ChangeListenerWithBlocker<Boolean>


    /**
     * Adds to a cell:
     *  - the converter,
     *  - listeners for a changed value,
     *  - drag and drop listeners,
     *  - what has to happen when editing,
     *  - context menu.
     *
     * @param progressIndicator User feedback.
     * @param gridPane Main UI element.
     * @param focusDay Focusday of UI.
     */
    fun setup(progressIndicator: ProgressIndicator, gridPane: GridPane, focusDay: LocalDate) {

        // Update text on changes.
        converter = TaskConverter(this)

        // Update course label/combobox on changes.
        val updater = OnCourseLabelChangeUpdater(progressIndicator, comboBox)
        updater.addValueChangeListener(this)
        labelChangeListener = updater.addDatabaseUpdaterChangeListener(tree, day)

        // If an item is selected, deselect all other items.
        SelectionCleaner(tree).addSelectionListener()

        doneChangeListener = CheckBoxUpdater(progressIndicator, checkBox).setChangeListener(tree, this, day)

        // Drag 'n Drop.
        DragDetect(this, tree.root)
        DragOver(this)
        DragEnter(this, tree)
        DragExit(this, tree)
        DragDrop(this, tree, day, progressIndicator)
        DragDone(this, tree, day, progressIndicator)

        SubtasksEditor(progressIndicator, tree, day).setup()

        // Create the context menu.
        contextMenu = ContextMenuCreator(gridPane, focusDay, progressIndicator, this, day).create()

    }

    /**
     * Sets layout of this.
     *
     * @param homeworkTask The Homework task to be displayed in the Cell.
     * @param empty Whether or not the new Cell displays data.
     */
    override fun updateItem(homeworkTask: HomeworkTask?, empty: Boolean) {
        super.updateItem(homeworkTask, empty)

        TaskLayout(this, doneChangeListener, checkBox, label, labelChangeListener, tree.root, comboBox).update(homeworkTask)
    }

}