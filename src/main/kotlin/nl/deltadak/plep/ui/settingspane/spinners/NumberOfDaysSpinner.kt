package nl.deltadak.plep.ui.settingspane.spinners

import javafx.scene.control.Spinner
import javafx.scene.control.SpinnerValueFactory
import javafx.scene.layout.GridPane
import nl.deltadak.plep.database.settingsdefaults.SettingsDefaults
import nl.deltadak.plep.database.tables.Settings
import nl.deltadak.plep.ui.settingspane.SPINNER_WIDTH

/**
 * This spinner allows the user to select the number of days that the main GridPane should show.
 */
class NumberOfDaysSpinner {

    /**
     * Construct a new spinner.
     */
    fun getNew(): Spinner<Int> {

        val spinner = Spinner<Int>()

        val initialNumberOfDays = Settings.get(SettingsDefaults.NUMBER_OF_DAYS).toInt()
        spinner.valueFactory = SpinnerValueFactory.IntegerSpinnerValueFactory(1, 31, initialNumberOfDays)
        spinner.id = "numberOfShowDaysSpinner"
        spinner.prefWidth = SPINNER_WIDTH // TODO Check width for double digits
        GridPane.setColumnIndex(spinner, 2)
        GridPane.setRowIndex(spinner, 1)

        return spinner

    }

}