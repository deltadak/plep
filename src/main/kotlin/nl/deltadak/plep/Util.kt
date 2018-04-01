@file:Suppress("unused")

package nl.deltadak.plep

/**
 * Find the minimum of Ints.
 *
 * @param ints The elements to find the minimum of.
 */
fun min(vararg ints: Int): Int {
    var result = ints[0]
    for (element in ints.drop(1)) {
        result = Math.min(result, element)
    }
    return result
}

/**
 * Find the minimum of Ints, but if the minimum is lower than 0 then the result is 0.
 *
 * @param ints The elements to find the minimum of.
 */
fun minZero(vararg ints: Int): Int {
    var result = ints[0]
    for (element in ints.drop(1)) {
        result = Math.min(result, element)
    }
    return Math.max(result, 0)
}
