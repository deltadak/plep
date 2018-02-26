@file:Suppress("UNUSED_ANONYMOUS_PARAMETER")

package deltadak.ui.settingspane

import deltadak.Database
import deltadak.database.DatabaseSettings
import deltadak.ui.Controller
import deltadak.ui.util.getNumberOfColumns
import javafx.scene.control.CheckBox
import javafx.scene.control.Spinner
import kotlin.reflect.KMutableProperty

/**
 * This checkbox indicates whether the number of columns should be calculated automatically.
 */
class AutoColumnsAction(
        /** The FXML reference to the checkbox. */
        val autoColumnsCheckBox: CheckBox,
        /** The FXML reference to the number of columns spinner. */
        val numberOfColumnsSpinner: Spinner<Int>) {

    /**
     * Temporary function to be called from Java, since that has no pass by reference for variables.
     */
    fun javaSet(refreshUI: () -> Unit, controller: Controller) {
        set(refreshUI, controller::numberOfDays)
    }

    /**
     * Set the action on toggle.
     * When checked, the number of columns will be calculated automatically.
     * When unchecked, the last known custom value will be used.
     * In both cases, the UI will be refreshed.
     *
     * @param refreshUI Should refresh UI when called.
     * @param numberOfDaysProperty Should be a reference to the number of days shown.
     */
    fun set(refreshUI: () -> Unit, numberOfDaysProperty: KMutableProperty<Int>) {

        autoColumnsCheckBox.selectedProperty().addListener {
            observable, oldValue, newValue ->

            // Updating the database is enough, since these values will be used when refreshing the UI.
            Database.INSTANCE.updateSetting(DatabaseSettings.MAX_COLUMNS_AUTO.settingsName, newValue.toString())

            // Update spinner, if the checkbox is checked then the calculated number of columns will be shown.
            numberOfColumnsSpinner.valueFactory.value =
            if (newValue) {
                // Calculate number of columns from number of days to be shown.
                getNumberOfColumns(numberOfDaysProperty.getter.call())
            } else {
                // The checkbox was unchecked, use the last known custom value.
                Database.INSTANCE.getSetting(DatabaseSettings.MAX_COLUMNS.settingsName).toInt()
            }

            refreshUI()

        }
    }
}
