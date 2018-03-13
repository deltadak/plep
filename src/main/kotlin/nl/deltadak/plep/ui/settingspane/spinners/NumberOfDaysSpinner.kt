package nl.deltadak.plep.ui.settingspane.spinners

import nl.deltadak.plep.database.DatabaseSettings
import nl.deltadak.plep.ui.settingspane.SPINNER_WIDTH
import javafx.scene.control.Spinner
import javafx.scene.control.SpinnerValueFactory
import javafx.scene.layout.GridPane
import nl.deltadak.plep.Database

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