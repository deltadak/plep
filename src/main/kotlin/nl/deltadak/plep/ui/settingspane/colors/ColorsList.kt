package nl.deltadak.plep.ui.settingspane.colors

import javafx.scene.control.ColorPicker
import nl.deltadak.plep.database.tables.Colors
import nl.deltadak.plep.ui.util.converters.toColor
import nl.deltadak.plep.ui.util.converters.toHex

/**
 * This class represents the few color pickers with which the user can change the colors to color a HomeworkTask with.
 */
class ColorsList(
        /** A list of FXML references to the ColorPickers to set up. */
        private val colorPickers: List<ColorPicker>) {

    /**
     * Set up the color pickers given.
     *
     * @param refreshUI Should refresh UI when called.
     */
    fun setup(refreshUI: () -> Unit) {
        // Get the colors from the database, so the colorpickers can be set to those colors.
        Colors.getAll().mapIndexed { index, color ->
            colorPickers[index].styleClass.add("button")
            // Set the color in the database on the color picker.
            colorPickers[index].value = color.toColor()
        }

        @Suppress("RedundantLambdaArrow") // Needed to have type inference for 'it'
        colorPickers.forEach { it.setOnAction{ _ -> editColor(it, refreshUI) }}

    }

    /**
     * Updates the value of the color in the database.
     *
     * @param colorPicker The ColorPicker that is edited.
     * @param refreshUI Should refresh UI when called.
     */
    private fun editColor(colorPicker: ColorPicker, refreshUI: () -> Unit) {
        val id = colorPickers.indexOf(colorPicker)
        Colors.update(id, colorPicker.value.toHex())
        refreshUI()
    }

}
