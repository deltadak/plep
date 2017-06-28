package deltadak.ui;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;

/**
 * A custom listener for the combo boxes which can be blocked,
 * with intended use case that you block the listener when setting
 * the value of a combo box programmatically.
 *
 * Using an {@code InvalidationListener} instead of a {@code ChangeListener} has some slight
 * performance improvement or so.
 */
class ListenerWithBlocker implements InvalidationListener {
    InvalidationListener decoratedListener;
    boolean block;
    
    /**
     * Constructor.
     * @param decoratedListener listener to set
     */
    public ListenerWithBlocker(InvalidationListener decoratedListener) {
        this.decoratedListener = decoratedListener;
        block = false;
    }
    
    /**
     * Execute listener passed in the constructor only if not blocked.
     * @param observable Observable to add listener to?
     */
    @Override
    public void invalidated(Observable observable) {
        if(!block) {
            decoratedListener.invalidated(observable);
        }
    }
    
    /**
     * Set whether the listener should be blocked.
     * @param block boolean
     */
    public void setBlock(boolean block) {
        this.block = block;
    }
}
