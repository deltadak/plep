package deltadak.ui.settingspane.spinners

import deltadak.Database
import deltadak.database.DatabaseSettings
import deltadak.ui.settingspane.SPINNER_WIDTH
import javafx.scene.control.Spinner
import javafx.scene.control.SpinnerValueFactory
import javafx.scene.layout.GridPane

/**
 * This spinner allows the user to select the number of days that the main GridPane should show.
 */
class NumberOfDaysSpinner {

    /**
     * Construct a new spinner.
     */
    fun getNew(): Spinner<Int> {

        val spinner = Spinner<Int>()

        val initialNumberOfDays = Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_DAYS.settingsName).toInt()
        spinner.valueFactory = SpinnerValueFactory.IntegerSpinnerValueFactory(1, 31, initialNumberOfDays)
        spinner.id = "numberOfShowDaysSpinner"
        spinner.prefWidth = SPINNER_WIDTH // todo check good for double digits?
        GridPane.setColumnIndex(spinner, 2)
        GridPane.setRowIndex(spinner, 1)

        return spinner

    }

}