package deltadak.ui.settingspane.applybuttons

import deltadak.Database
import deltadak.database.DatabaseSettings
import deltadak.ui.Controller
import javafx.scene.control.Button
import javafx.scene.control.Spinner
import kotlin.reflect.KMutableProperty

/**
 * This button applies the selected number of days to be shown.
 */
class ApplyNumberOfDaysAction(
        /** The FXML reference to the button. */
        val applyNumberOfDaysButton: Button,
        /** The FXML reference to the spinner. */
        val numberOfDaysSpinner: Spinner<Int>) {

    /**
     * Temporary function to be called from Java, since that has no pass by reference for variables.
     */
    fun javaSet(controller: Controller, refreshUI: () -> Unit) {
        set(controller::numberOfDays, refreshUI)
    }

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
            Database.INSTANCE.updateSetting(DatabaseSettings.NUMBER_OF_DAYS.settingsName, numberOfDays.toString())

            // Update UI.
            refreshUI()

        }

    }

}
