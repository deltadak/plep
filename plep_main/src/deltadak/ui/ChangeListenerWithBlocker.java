package deltadak.ui;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

/**
 * A custom listener for the checkboxes which can be blocked.
 * You should use this to block the listener on the checkbox when
 * programmatically setting its value.
 *
 * Using a {@code ChangeListener} because we need to have access to the new
 * value.
 *
 * @param <B> Boolean, the ChangeListener needs this...
 */
public class ChangeListenerWithBlocker<B> implements ChangeListener<Boolean> {
    
    private ChangeListener<Boolean> decoratedListener;
    private boolean block = false;
    
    /**
     * Constructor.
     * @param decoratedListener listener to set
     */
    ChangeListenerWithBlocker(ChangeListener<Boolean> decoratedListener) {
        this.decoratedListener = decoratedListener;
    }
    
    /**
     * Blocks or unblocks the listener.
     * @param block boolean is true when blocking the listener.
     */
    public void setBlock(boolean block) {
        this.block = block;
    }
    
    /**
     * Execute the listener passed in the constructor only of not blocked.
     * @param observable ObservableValue to add listener to?
     * @param oldValue The old value that has been changed.
     * @param newValue The new value the old value has been changed to.
     */
    @Override
    public void changed(ObservableValue<? extends Boolean> observable, Boolean
            oldValue, Boolean newValue) {
        if(!block) {
            decoratedListener.changed(observable, oldValue, newValue);
        }
    }
}
