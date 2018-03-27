package nl.deltadak.plep.ui.taskcell.treecell

import javafx.scene.control.TreeCell
import javafx.util.StringConverter
import nl.deltadak.plep.HomeworkTask

/**
 * Custom stringconverter to define what editing a [TreeCell] means.
 * This converter is for one TreeCell.
 */
class TaskConverter(
        /** TreeCell for which the converter is. */
        private val cell: TreeCell<HomeworkTask>) : StringConverter<HomeworkTask>() {

    /**
     * Converts the HomeworkTask provided into its string form.
     */
    override fun toString(homeworkTask: HomeworkTask?): String = homeworkTask?.text ?: ""

    /**
     * Converts a string into a HomeworkTask.
     */
    override fun fromString(string: String): HomeworkTask = cell.item.apply { text = string }

}
