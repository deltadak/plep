package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TreeItem;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;

import javax.print.DocFlavor;
import java.util.ArrayList;

/**
 * @Author s152337 14-4-2017
 */
public class CustomTreeCell extends TextFieldTreeCell<HomeworkTask> {
    
    private ObservableList<String> comboList;
    private TreeItem<HomeworkTask> root;
    
    private HBox cellBox;
    private CheckBox checkBox;
    private Label label;
    private ComboBox<String> comboBox;
    
    CustomTreeCell(TreeItem<HomeworkTask> root) {
        this.root = root;
        comboList = FXCollections
                .observableArrayList(Database.INSTANCE.getLabels());
        comboBox = new ComboBox<>(comboList);
        setConverter(new TaskConverter(this));
    }
    
    
    /**
     * Sets the layout of a TreeCell. Always contains a CheckBox and
     * a Label (text). Also contains a ComboBox if the Cell is the root of a
     * task.
     * @param item The Homework task to be displayed in the Cell.
     * @param empty Whether or not the new Cell displays data.
     */
    @Override
    public void updateItem(HomeworkTask item, boolean empty) {
        super.updateItem(item, empty);
        
        if(isEmpty()) {
            setGraphic(null);
            setText(null);
        } else {
            cellBox = new HBox(10);
            CheckBox checkBox = new CheckBox();
            label = new Label(item.getText());
//            getTreeItem().getParent().equals(root);
            if(getTreeItem().getParent().equals(root)) {

                ComboBox<String> comboBox = new ComboBox<>(comboList);
                Region region = new Region();
                HBox.setHgrow(region, Priority.ALWAYS);

                cellBox.getChildren().addAll(checkBox, label, region, comboBox);

                setGraphic(cellBox);
                setText(null);
            } else {
                cellBox.getChildren().addAll(checkBox, label);
                setGraphic(cellBox);
                setText(null);
            }
        }
    }
    
    
}
