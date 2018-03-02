package deltadak.ui.settingspane.spinners

import deltadak.Database
import deltadak.database.DatabaseSettings
import deltadak.ui.settingspane.SPINNER_WIDTH
import javafx.scene.control.Spinner
import javafx.scene.control.SpinnerValueFactory
import javafx.scene.layout.GridPane

/**
 * This spinner allows the user to select the number of days that the forward/backward buttons should skip.
 */
class NumberOfMovingDaysSpinner {

    /**
     * Construct a new spinner.
     */
    fun getNew(): Spinner<Int> {

        val spinner = Spinner<Int>()

        val initialNumberOfMovingDays = Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_MOVING_DAYS.settingsName).toInt()
        spinner.valueFactory = SpinnerValueFactory.IntegerSpinnerValueFactory(1, 14, initialNumberOfMovingDays)
        spinner.id = "numberOfMovingDaysSpinner"
        spinner.prefWidth = SPINNER_WIDTH
        GridPane.setColumnIndex(spinner, 2)

        return spinner

    }

}