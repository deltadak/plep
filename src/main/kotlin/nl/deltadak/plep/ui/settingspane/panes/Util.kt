package nl.deltadak.plep.ui.settingspane.panes

import javafx.scene.Node

/**
 * Returns whether a MouseEvent happened in a certain node or not.
 *
 * @param eventNode The node the event happened in.
 * @param potentialHierarchyElement The node to check if the event
 * happened in.
 * @return True if the event happened in the checked node, false
 * otherwise.
 */
fun isInHierarchy(eventNode: Node, potentialHierarchyElement: Node): Boolean {
    var node = eventNode
    // Return if any parent of the node is the potentialHierarchyElement.
    // Assuming that the hierarchy is finite, this while loop is guaranteed to terminate because then at some point node will not have a parent anymore.
    while (node != potentialHierarchyElement) {
        // If node has no parent anymore, return false.
        node = node.parent ?: return false
    }
    return true
}

/**
 * Enables a node, to circumvent double negation.
 */
fun Node.enable() {
    isDisable = false
}

/**
 * Disables a node.
 */
fun Node.disable() {
    isDisable = true
}
