package nl.deltadak.plep.ui.settingspane.applybuttons

import javafx.scene.control.Button
import javafx.scene.control.Spinner
import nl.deltadak.plep.database.settingsdefaults.SettingsDefaults
import nl.deltadak.plep.database.tables.Settings
import kotlin.reflect.KMutableProperty

/**
 * This button applies the selected number of days to be shown.
 */
class ApplyNumberOfDaysAction(
        /** The FXML reference to the button. */
        private val applyNumberOfDaysButton: Button,
        /** The FXML reference to the spinner. */
        private val numberOfDaysSpinner: Spinner<Int>) {

    /**
     * Set the button action.
     *
     * @param numberOfDaysProperty Should point to the number of days to be shown.
     * @param refreshUI Should refresh UI when called.
     */
    fun set(numberOfDaysProperty: KMutableProperty<Int>, refreshUI: () -> Unit) {

        applyNumberOfDaysButton.setOnAction {

            // Get current user-selected value.
            val numberOfDays = numberOfDaysSpinner.value

            // Update property.
            numberOfDaysProperty.setter.call(numberOfDays)

            // Update database.
            Settings.update(SettingsDefaults.NUMBER_OF_DAYS, numberOfDays.toString())

            // Update UI.
            refreshUI()

        }

    }

}
