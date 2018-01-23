package deltadak.ui.taskcell;

import deltadak.HomeworkTask;
import javafx.scene.control.ListCell;
import javafx.scene.control.TreeCell;
import javafx.util.StringConverter;

/**
 * custom stringconverter to define what editing a listcell means
 * this converter is set on each listcell
 */
public class TaskConverter extends StringConverter<HomeworkTask> {
    
    private final TreeCell<HomeworkTask> cell;
    
    TaskConverter(final TreeCell<HomeworkTask> cell) {
        this.cell = cell;
    }
    
    @Override
    public String toString(final HomeworkTask homeworkTask) {
        return homeworkTask.getText();
    }
    
    @Override
    public HomeworkTask fromString(final String string) {
        HomeworkTask homeworkTask = cell.getItem();
        homeworkTask.setText(string);
        
        return homeworkTask;
    }
}
