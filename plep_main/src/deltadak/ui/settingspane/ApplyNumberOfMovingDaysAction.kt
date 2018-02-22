package deltadak.ui.settingspane

import deltadak.Database
import deltadak.database.DatabaseSettings
import deltadak.ui.Controller
import deltadak.ui.SlidingSettingsPane
import javafx.scene.control.Button
import javafx.scene.control.Spinner
import kotlin.reflect.KMutableProperty

/**
 * This button applies the selected number of days to be shown.
 */
class ApplyNumberOfMovingDaysAction(
        /** The FXML reference to the button. */
        val applyNumberOfDaysButton: Button,
        /** The FXML reference to the spinner. */
        val numberOfMovingDaysSpinner: Spinner<Int>) {

    /**
     * Temporary function to be called from Java, since that has no pass by reference for variables.
     */
    fun javaSet(controller: Controller, refreshUI: () -> Unit) {
        set(controller::numberOfMovingDays, refreshUI)
    }

    /**
     * Set the button action.
     *
     * @param numberOfMovingDaysProperty Should point to the number of days that the forward and backward button skip when pressed.
     */
    fun set(numberOfMovingDaysProperty: KMutableProperty<Int>, refreshUI: () -> Unit) {

        applyNumberOfDaysButton.setOnAction {

            // Get current user-selected value.
            val numberOfMovingDays = numberOfMovingDaysSpinner.value

            // Update property.
            numberOfMovingDaysProperty.setter.call(numberOfMovingDays)

            // Update database.
            Database.INSTANCE.updateSetting(DatabaseSettings.NUMBER_OF_MOVING_DAYS.settingsName, numberOfMovingDays.toString())

            // Update UI.
            refreshUI()

        }

    }

}