package nl.deltadak.plep.commands;

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
        executionHook();
    }

    /**
     * Requires subclass to implement execute(),
     * but at least does not require them to not forget
     * to call super.execute(), conform Template pattern.
     */
    protected abstract void executionHook();

    /**
     * Undo
     */
    public void undo() {
        executed = false;
        undoHook();
    }

    /**
     * Same reason as with {@link #executionHook()} )}.
     */
    protected abstract void undoHook();

    /**
     * @return whether command is executed
     */
    public boolean isExecuted() {
        return executed;
    }

}
