package nl.deltadak.plep.ui.taskcell.components.textlabel

import javafx.scene.control.ComboBox
import javafx.scene.control.Label

/**
 * This class provides options to change the style of the text label during runtime for some predefined styles.
 */
class TextLabelStyle {

    /**
     * Sets the style of the text of a task, depending on whether the task
     * is done or not. Styles are defined in the CSS.
     *
     * @param done boolean, true if the task is done, false if not done.
     * @param label The text label of which to change style.
     * @param comboBox The combo box of which to change style.
     */
    fun setDoneStyle(done: Boolean, label: Label, comboBox: ComboBox<String>) {

        if (done) {
            label.styleClass.remove("label")
            label.styleClass.add("label-done")
            comboBox.styleClass.remove("combo-box")
            comboBox.styleClass.add("combobox-done")

        } else {
            // Remove all the classes which styled the 'done' style on the item.
            label.styleClass.remove("label-done")
            label.styleClass.add("label")

            comboBox.styleClass.remove("combobox-done")
            comboBox.styleClass.add("combo-box")

        }

    }

}