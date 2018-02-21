@file:Suppress("KDocMissingDocumentation")

package deltadak.ui.global

import java.time.LocalDate

/*
 * In this file, global layout variables are modeled as data classes, so the references can be passed to any function, which then can access the value at the right time (in a click listener, for example) but at the same time dummies are easily provided for those functions for testing, compared to when using a plain global variable.
 */

/**
 * This class holds the current value of the number of days shown in the UI.
 */
data class NumberOfDays(
        val currentValue: Int)

/**
 * Day on which the gridpane is 'focused': the second day shown will be this day.
 */
data class FocusDay(
        val currentValue: LocalDate
)
