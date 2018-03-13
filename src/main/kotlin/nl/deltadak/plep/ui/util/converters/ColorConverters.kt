package nl.deltadak.plep.ui.util.converters

import javafx.scene.paint.Color

/**
 * Convert a string containing e.g. hex code like "#ff6688" to a Color.
 */
fun String.toColor() : Color = Color.web(this)

/**
 * Converts a Color to a string hex value.
 */
fun Color.toHex() : String = toString().substring(2,8)
