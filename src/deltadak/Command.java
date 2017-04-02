package deltadak;

/**
 * Default command, conform the command pattern.
 */
public abstract class Command {

    /**
     * make sure not to be executed twice
     */
    private boolean executed;

    /**
     * Execute command
     * @throws IllegalStateException when already executed
     */
    public void execute() throws IllegalStateException {
        if (executed) {
            throw new IllegalStateException("Cannot execute command twice");
        }
        executed = true;
        continueExecution();
    }

    /**
     * Requires subclass to implement execute(),
     * but at least does not require them to not forget
     * to call super.execute(), conform Template pattern.
     */
    protected abstract void continueExecution();

    /**
     * Undo
     */
    public void undo() {
        executed = false;
    }

    /**
     * Same reason as with {@link #continueExecution()} )}.
     */
    protected abstract void continueUndo();

    /**
     * @return whether command is executed
     */
    public boolean isExecuted() {
        return executed;
    }

}
