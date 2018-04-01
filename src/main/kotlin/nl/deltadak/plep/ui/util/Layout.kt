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
        "ff1a00",
        "00cbef",
        "7df202",
        "f444a7",
        "ffffff"
)
