package nl.deltadak.plep.ui.taskcell.blockerlisteners

import javafx.beans.InvalidationListener
import javafx.beans.Observable

/**
 * A custom listener for the combo boxes which can be blocked, with intended use case that you block the listener when setting the value of a combo box programmatically.
 *
 * Using an {@code InvalidationListener} instead of a {@code ChangeListener} has some slight performance improvement or so.
 */
class InvalidationListenerWithBlocker(
        /** Listener to block or not. */
        private val decoratedListener: InvalidationListener) : InvalidationListener {

    /** Whether to block the listener or not. */
    var block = false

    /**
     * Execute listener passed in the constructor only if not blocked.
     * @param observable Observable to add listener to?
     */
    override fun invalidated(observable: Observable) {
        if (!block) {
            decoratedListener.invalidated(observable)
        }
    }

}
