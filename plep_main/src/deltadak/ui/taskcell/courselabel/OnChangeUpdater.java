package deltadak.ui.taskcell.courselabel;

import deltadak.HomeworkTask;
import javafx.application.Platform;
import javafx.scene.control.ComboBox;
import javafx.scene.control.cell.TextFieldTreeCell;

/**
 * Manages the updating of the combo box on changes.
 */
public class OnChangeUpdater {

    private ComboBox<String> comboBox;
    private TextFieldTreeCell<HomeworkTask> treeCell;

    /**
     * Allows for more control over what happens when a new value of the combo box is chosen.
     * @param comboBox the combo box which to control
     * @param treeCell the TreeCell in which the combobox resides
     */
    public OnChangeUpdater(ComboBox<String> comboBox, TextFieldTreeCell<HomeworkTask> treeCell) {
        this.comboBox = comboBox;
        this.treeCell = treeCell;
    }

    /**
     * Update the text shown on the combo box where there are changes, with some customizations.
     */
    public void setupListener() {
        if ((comboBox != null) && (treeCell != null)) {
            comboBox.valueProperty().addListener(
                    (observable, oldValue, newValue) -> {

                        HomeworkTask task = treeCell.getTreeItem().getValue();

                        if (newValue.equals("<no label>")) {
                            task.setLabel("");
                            // Delay removing the combobox text because we cannot change the contents of an ObservableList while a change is in progress.
                            // In practice the delay is unnoticable.
                            Platform.runLater(() -> comboBox.setValue(""));
                        } else {
                            task.setLabel(newValue);
                        }
                    }
            );
        }
    }
}
