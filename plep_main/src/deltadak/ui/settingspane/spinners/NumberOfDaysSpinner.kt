package deltadak.ui.settingspane.spinners

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
     *
     * @param initialNumberOfDays Initial number to be shown on the spinner.
     */
    fun getNew(initialNumberOfDays: Int): Spinner<Int> {

        val spinner = Spinner<Int>()

        spinner.valueFactory = SpinnerValueFactory.IntegerSpinnerValueFactory(1, 31, initialNumberOfDays)
        spinner.id = "numberOfShowDaysSpinner"
        spinner.prefWidth = SPINNER_WIDTH // todo check good for double digits?
        GridPane.setColumnIndex(spinner, 2)
        GridPane.setRowIndex(spinner, 1)

        return spinner

    }

}