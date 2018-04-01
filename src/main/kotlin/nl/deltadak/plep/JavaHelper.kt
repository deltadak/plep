package nl.deltadak.plep

/**
 * Because Java cannot call top level Kotlin fields. Can be removed if no callers left. See [nl.deltadak.plep.ui.util.DEFAULT_COLORS].
 */
class JavaHelper {
    companion object {
        /** */
        @JvmField val DEFAULT_COLORS = arrayOf(
                "ee5e5e", // Red.
                "5e78e5", // Blue.
                "60ed79", // Green.
                "ffd253", // Orange.
                "eee3ff" // Very light purple.
        )
    }
}
