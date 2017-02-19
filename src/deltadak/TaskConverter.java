package deltadak;

import javafx.scene.control.ListCell;
import javafx.util.StringConverter;

/**
 * custom stringconverter to define what editing a listcell means
 * this converter is set on each listcell
 */
class TaskConverter extends StringConverter<Task> {
    
    private final ListCell<Task> cell;
    
    TaskConverter(final ListCell<Task> cell) {
        this.cell = cell;
    }
    
    @Override
    public String toString(final Task task) {
        return task.getText();
    }
    
    @Override
    public Task fromString(final String string) {
        Task task = cell.getItem();
        task.setText(string);
        
        return task;
    }
}
