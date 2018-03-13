package nl.deltadak.plep.database

/**
 * The names of some settings in the database.
 */
enum class DatabaseSettings(
        /** Name. */
        val settingsName: String) {
    /** The number of days shown. */
    NUMBER_OF_DAYS("number_of_days"),
    /** The number of days which the left/right buttons go drawkcab/forward */
    NUMBER_OF_MOVING_DAYS("number_of_moving_days"),
    /** The maximum number of columns. */
    MAX_COLUMNS("max_columns"),
    /** Whether the number of columns should be calculated automatically. */
    MAX_COLUMNS_AUTO("max_columns_auto"),
}
