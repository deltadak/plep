package nl.deltadak.plep.ui.util

/**
 * Calculates the 'ideal' number of columns, given a certain number of days to show.
 *
 * @param numberOfDays Total number of days.
 *
 * @return Number of columns.
 */
fun getNumberOfColumns(numberOfDays: Int): Int = Math.ceil(Math.sqrt(numberOfDays.toDouble())).toInt()

/** Default (initial) colors */
val DEFAULT_COLORS = arrayOf(
        "ee5e5e", // Red.
        "5e78e5", // Blue.
        "60ed79", // Green.
        "ffd253", // Orange.
        "F4F1FA" // Very light purple.
)
