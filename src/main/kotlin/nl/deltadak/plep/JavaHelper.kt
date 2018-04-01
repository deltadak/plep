package nl.deltadak.plep

/**
 * Because Java cannot call top level Kotlin fields. Can be removed if no callers left. See [nl.deltadak.plep.ui.util.DEFAULT_COLORS].
 */
class JavaHelper {
    companion object {
        /** */
        @JvmField val DEFAULT_COLORS = arrayOf(
                "ff1a00",
                "00cbef",
                "7df202",
                "f444a7",
                "ffffff"
        )
    }
}
