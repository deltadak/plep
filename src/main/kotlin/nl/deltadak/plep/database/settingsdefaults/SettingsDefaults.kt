package nl.deltadak.plep.database.settingsdefaults

/**
 * The names of some settings in the database.
 *
 * Note that the name of a value has to equal the settingsName converted to uppercase.
 */
enum class SettingsDefaults(
        /** Name. */
        val settingsName: String,
        /** Default value. */
        val default: String) {
    /** The number of days shown. */
    NUMBER_OF_DAYS("number_of_days", "9"),
    /** The number of days which the left/right buttons go drawkcab/forward. */
    NUMBER_OF_MOVING_DAYS("number_of_moving_days", "7"),
    /** The maximum number of columns. */
    MAX_COLUMNS("max_columns", "3"),
    /** Whether the number of columns should be calculated automatically. */
    MAX_COLUMNS_AUTO("max_columns_auto", "true"),
    /** To check whether a database exists, because Exposed doesn't provide a method for that. */
    TEST("test", "yes")
}
