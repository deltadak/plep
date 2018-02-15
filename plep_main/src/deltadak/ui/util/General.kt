@file:Suppress("unused", "PackageName")

package deltadak.ui.util

/**
 * Equals which returns true when both compared objects are null and is null safe.
 */
fun Any?.safeEquals(other: Any?): Boolean {
    if (this == null && other == null) {
        return true
    }
    if (this == null) {
        return false
    }
    return this == other
}
