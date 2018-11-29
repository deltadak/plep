package nl.deltadak.plep.ui.settingspane.spinners

import nl.deltadak.plep.database.namesanddefaults.DatabaseSettings
import nl.deltadak.plep.ui.settingspane.SPINNER_WIDTH
import javafx.scene.control.Spinner
import javafx.scene.control.SpinnerValueFactory
import javafx.scene.layout.GridPane
import nl.deltadak.plep.Database

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