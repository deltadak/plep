package nl.deltadak.plep.ui.taskcell

import javafx.geometry.Pos
import javafx.scene.control.CheckBox
import javafx.scene.control.ComboBox
import javafx.scene.control.Label
import javafx.scene.control.TreeItem
import javafx.scene.layout.HBox
import javafx.scene.layout.Priority
import javafx.scene.layout.Region
import javafx.scene.text.TextAlignment
import nl.deltadak.plep.HomeworkTask
import nl.deltadak.plep.database.tables.Colors
import nl.deltadak.plep.ui.taskcell.components.textlabel.TextLabelStyle
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.ChangeListenerWithBlocker
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.InvalidationListenerWithBlocker
import nl.deltadak.plep.ui.util.LABEL_MAGIK

/**
 * Manages the layout of a TreeCell. Always contains a CheckBox and a Label (text). Also contains a ComboBox (dropdown menu) if the Cell is the root of a task.
 * The given components are updated with the correct layout.
 *
 * @property taskCell Cell to set layout on.
 * @property doneChangeListener Should provide the ability to temporarily block the listener of the checkbox.
 * @property checkBox Checkbox of the task.
 * @property label Text of the task.
 * @property labelChangeListener Should provide the ability to temporarily block the listener of the combobox.
 * @property root Root item of the TreeView in which the task is.
 * @property comboBox Combobox of the task.
 */
class TaskLayout(private val taskCell: TaskCell, private val doneChangeListener: ChangeListenerWithBlocker<Boolean>, private val checkBox: CheckBox, val label: Label, private val labelChangeListener: InvalidationListenerWithBlocker, val root: TreeItem<HomeworkTask>, private val comboBox: ComboBox<String>) {

    /**
     * Updates the layout.
     * The homeworktask could be null, since updating is initiated by JavaFX.
     *
     * @param homeworkTask The Homework task to be displayed in the Cell.
     */
    fun update(homeworkTask: HomeworkTask?) {

        if (taskCell.isEmpty) {
            taskCell.graphic = null
            taskCell.text = null
        } else {
            if (homeworkTask != null) {
                setLayout(homeworkTask)
            }
        }

    }

    private fun setLayout(homeworkTask: HomeworkTask) {

        // Create the container for components.
        val cellBox = HBox(10.0)
        cellBox.alignment = Pos.CENTER_LEFT

        setCheckBox(homeworkTask)
        setTextLabel(homeworkTask)

        // Get style from the database and apply to the item.
        val color = Colors.get(homeworkTask.colorID)
        taskCell.style = "-fx-control-inner-background: #$color"
        TextLabelStyle().setDoneStyle(homeworkTask.done, label, comboBox)

        // If the item is top level, a parent task, it has to show a course label (ComboBox), and it has to have a context menu.
        if (taskCell.treeItem.parent == root) {
            val region = setComboBox(homeworkTask)
            cellBox.children.addAll(checkBox, label, region, comboBox)

            taskCell.graphic = cellBox
            taskCell.text = null
        } else {
            // Set up subtask, with context menu disabled.
            with(taskCell) {
                contextMenu = null
                graphic = cellBox.apply { children.addAll(checkBox, label) }
                text = null
            }
        }

    }

    /**
     * Setup checkbox.
     */
    private fun setCheckBox(homeworkTask: HomeworkTask) {
        // Block the listener on the checkbox when we manually toggle it, so it corresponds to the value in the database.
        doneChangeListener.block = true

        val done = homeworkTask.done
        // Manually toggle, if necessary.
        checkBox.isSelected = done

        doneChangeListener.block = false
    }

    /**
     * Setup text label.
     */
    private fun setTextLabel(homeworkTask: HomeworkTask) {
        with(label) {
            text = homeworkTask.text
            prefWidthProperty().bind(taskCell.treeView.widthProperty().subtract(LABEL_MAGIK))
            isWrapText = true
            textAlignment = TextAlignment.JUSTIFY
        }
    }

    /**
     * Setup combobox.
     */
    private fun setComboBox(homeworkTask: HomeworkTask): Region {
        // Before setting value, we need to temporarily disable the listener, otherwise it fires and goes unnecessarily updating the database, which takes a lot of time.
        labelChangeListener.block = true
        comboBox.value = homeworkTask.label
        labelChangeListener.block = false

        // Create a region to make sure that the ComboBox is aligned  on the right.
        val region = Region()
        HBox.setHgrow(region, Priority.ALWAYS)
        return region
    }

}
