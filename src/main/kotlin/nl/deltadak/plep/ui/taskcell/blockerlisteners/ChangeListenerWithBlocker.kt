package nl.deltadak.plep.ui.taskcell.blockerlisteners

import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue

/**
 * A custom listener for the checkboxes which can be blocked.
 * You should use this to block the listener on the checkbox when
 * programmatically setting its value.
 *
 * Using a {@code ChangeListener} because we need to have access to the new value.
 *
 */
class ChangeListenerWithBlocker<Boolean> (
        /** The listener to block or unblock. */
        private var decoratedListener: ChangeListener<Boolean>) : ChangeListener<Boolean> {

    /** Whether the listener should be blocked. */
    var block = false

    /**
     * Execute the listener passed in the constructor only of not blocked.
     * @param observable ObservableValue to add listener to?
     * @param oldValue The old value that has been changed.
     * @param newValue The new value the old value has been changed to.
     */
    override fun changed(observable: ObservableValue<out Boolean>, oldValue: Boolean, newValue: Boolean) {
        if (!block) {
            decoratedListener.changed(observable, oldValue, newValue)
        }
    }

}
