package nl.deltadak.plep.ui.settingspane.applybuttons

import javafx.scene.control.Button
import javafx.scene.control.CheckBox
import javafx.scene.control.Spinner
import nl.deltadak.plep.database.settingsdefaults.SettingsDefaults
import nl.deltadak.plep.database.tables.Settings

/**
 * This button applies the selected number of columns (using the given Spinner) to the user interface.
 */
class ApplyNumberOfColumnsAction(
        /** The FXML reference to the button. */
        private val applyNumberOfColumnsButton: Button,
        /** The FXML reference to the spinner. */
        private val numberOfColumnsSpinner: Spinner<Int>,
        /** The FXML reference to the checkbox which selects whether the number of columns should be calculated automatically. */
        private val autoColumnsCheckBox: CheckBox) {

    /**
     * When called, the user has selected a custom number of columns overriding the automatic calculation of that number.
     *
     * @param refreshUI Should refresh UI when called.
     */
    fun set(refreshUI: () -> Unit) {

        applyNumberOfColumnsButton.setOnAction {

            val nrColumns = numberOfColumnsSpinner.value

            // Store the new custom value.
            Settings.update(SettingsDefaults.MAX_COLUMNS, nrColumns.toString())

            // User has overridden the automatic calculation.
            autoColumnsCheckBox.isSelected = false

            refreshUI()

        }
    }
}
