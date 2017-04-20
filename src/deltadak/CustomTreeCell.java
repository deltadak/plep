package deltadak;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;

import javax.print.DocFlavor;
import java.awt.event.MouseEvent;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author s152337 14-4-2017
 */
public class CustomTreeCell extends TextFieldTreeCell<HomeworkTask> {
    
    private ObservableList<String> comboList;
    private TreeItem<HomeworkTask> root;
    private Controller controller;
    
    private HBox cellBox;
    private CheckBox checkBox;
    private Label label;
    private ComboBox<String> comboBox;
    
    /**
     * Each LabelCell keeps a reference to the listener of the
     * ComboBox, in order to choose whether to block it temporarily or not.
     */
    ListenerWithBlocker labelChangeListener;
    
    CustomTreeCell(Controller controller, TreeItem<HomeworkTask> root) {
        this.controller = controller;
        this.root = root;
        comboList = FXCollections
                .observableArrayList(Database.INSTANCE.getLabels());
        comboBox = new ComboBox<>(comboList);
    }
    
    public void setup(TreeView<HomeworkTask> tree, LocalDate localDate) {
        setConverter(new TaskConverter(this));
        
        comboBox.valueProperty().addListener(
                (observable, oldValue, newValue) -> this.getTreeItem()
                    .getValue().setLabel(newValue)
        );
        
        setOnLabelChangeListener(tree, localDate);
        
        setContextMenu(createContextMenu(tree, localDate));
        
    }
    
    /**
     * Sets the layout of a TreeCell. Always contains a CheckBox and
     * a Label (text). Also contains a ComboBox if the Cell is the root of a
     * task.
     * @param homeworkTask The Homework task to be displayed in the Cell.
     * @param empty Whether or not the new Cell displays data.
     */
    @Override
    public void updateItem(HomeworkTask homeworkTask, boolean empty) {
        super.updateItem(homeworkTask, empty);
        
        if(isEmpty()) {
            setGraphic(null);
            setText(null);
        } else {
            cellBox = new HBox(10);
            checkBox = new CheckBox();
            label = new Label(homeworkTask.getText());
//            getTreeItem().getParent().equals(root);
            if(getTreeItem().getParent().equals(root)) {
    
                // Before setting value, we need to temporarily disable the
                // listener, otherwise it fires and goes unnecessarily updating
                // the database, which takes a lot of time.
                labelChangeListener.setBlock(true);
                comboBox.setValue((homeworkTask.getLabel() != null) ?
                                  homeworkTask.getLabel() : "<null>");
                labelChangeListener.setBlock(false);
                
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
            setStyle("-fx-control-inner-background: "
            + controller.convertColorToHex(homeworkTask.getColor()));
        }
    }
    
    /**
     * set listener on the ComboBox to update the database when the
     * selected index changes
     *
     * @param tree TreeView which the LabelCell is in, needed for updating
     *             the database
     * @param day LocalDate which we need for updating the database
     */
    void setOnLabelChangeListener(TreeView<HomeworkTask> tree,
                                  LocalDate day) {
        
        InvalidationListener invalidationListener = new InvalidationListener() {
            @Override
            public void invalidated(Observable observable) {
                controller.updateDatabase(day, controller
                        .convertTreeItemListToArrayList(
                                tree.getRoot().getChildren()));
                // We do not need to cleanup here, as no tasks
                // were added or deleted.
            }
        };
        
        // Pass the invalidationlistener on to the custom listener
        labelChangeListener = new ListenerWithBlocker(invalidationListener);
        
        // update label in database when selecting a different one
        comboBox.getSelectionModel().selectedIndexProperty()
                .addListener(labelChangeListener);
    }
    
    ContextMenu createContextMenu(final TreeView<HomeworkTask> tree,
                           final LocalDate day) {
        
        ContextMenu contextMenu = new ContextMenu();
        Menu repeatTasksMenu = makeRepeatMenu(this, day);
        SeparatorMenuItem separatorMenuItem = new SeparatorMenuItem();
        
        MenuItem firstColor = new MenuItem("Green");
        MenuItem secondColor = new MenuItem("Blue");
        MenuItem thirdColor = new MenuItem("Red");
        MenuItem defaultColor = new MenuItem("White");
        
        contextMenu.getItems().addAll(repeatTasksMenu, separatorMenuItem,
                                      firstColor, secondColor,
                                      thirdColor, defaultColor);
        
        for (int i = 1; i < contextMenu.getItems().size(); i++) {
            MenuItem colorMenuItem = contextMenu.getItems().get(i);
            colorMenuItem.setOnAction(event1 -> {
                System.out.println(colorMenuItem.getText() + " clicked");
                controller.setBackgroundColor(colorMenuItem, this);
                controller.updateDatabase(day,
                               controller.convertTreeItemListToArrayList(
                                       tree.getRoot().getChildren()));
                controller.cleanUp(tree);
                
            });
        }
        return contextMenu;
//        contextMenu.show(customTreeCell, event.getScreenX(), event.getScreenY());
    }
    
    private Menu makeRepeatMenu(CustomTreeCell customTreeCell, LocalDate day) {
        Menu repeatTasksMenu = new Menu("Repeat for x weeks");
        for (int i = 1; i < 9; i++) {
            MenuItem menuItem = new MenuItem(String.valueOf(i));
            repeatTasksMenu.getItems().add(menuItem);
        }
        
        List<MenuItem> repeatMenuItems = repeatTasksMenu.getItems();
        for (MenuItem repeatMenuItem : repeatMenuItems) {
            repeatMenuItem.setOnAction(event12 -> {
                int repeatNumber = Integer.valueOf(repeatMenuItem.getText());
                System.out.println(repeatNumber + " clicked");
                HomeworkTask homeworkTaskToRepeat = customTreeCell.getItem();
                repeatTask(repeatNumber, homeworkTaskToRepeat, day);
            });
        }
        return repeatTasksMenu;
    }
    
    private void repeatTask(final int repeatNumber, final HomeworkTask homeworkTask, LocalDate day) {
        for (int i = 0; i < repeatNumber; i++) {
            day = day.plusWeeks(1);
            List<HomeworkTask> homeworkTasks = controller.getDatabaseSynced
                    (day);
            homeworkTasks.add(homeworkTask);
            controller.updateDatabase(day, homeworkTasks);
        }
        controller.refreshAllDays();
    }
}
