package deltadak.ui.settingspane.spinners

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
    fun getNew(initialNumberOfMovingDays: Int): Spinner<Int> {

        val spinner = Spinner<Int>()
        spinner.valueFactory = SpinnerValueFactory.IntegerSpinnerValueFactory(1, 14, initialNumberOfMovingDays)
        spinner.id = "numberOfMovingDaysSpinner"
        spinner.prefWidth = SPINNER_WIDTH
        GridPane.setColumnIndex(spinner, 2)

        return spinner

    }

}