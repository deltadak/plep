package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.util.Callback;

import java.io.Serializable;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.function.BiFunction;
import java.util.function.Function;

import static java.lang.Integer.min;

public class Controller implements Initializable {
    @FXML ListView<Task> day1;
    @FXML ListView<String> day2;
    @FXML GridPane gridpane;
    DataFormat dataFormat = new DataFormat("com.deltadak.Task");

    /**
     * Initialization method for the controller.
     */
    @FXML public void initialize(URL location, ResourceBundle resourceBundle){
        setupGridPane();
    }

    /**
     * database communication below
     */



    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     */
    private void setupGridPane() {

        //some debug defaults
        ObservableList<Task> day1Tasks = FXCollections.observableArrayList(
                new Task("task1","2WA70"),
                new Task("task2","0LAUK0")
        );
//        ObservableList<String> day2List = FXCollections.observableArrayList("task3","task4","","","","");
        day1.setItems(day1Tasks);
//        day2.setItems(day2List);

        //setup drag and drop for all children of gridview
        gridpane.getChildren().stream().filter(node -> node instanceof ListView).forEach(node -> {
            ListView<Task> list = (ListView<Task>) node;
            setupListView(list);
            list.setOnEditCommit(t -> list.getItems().set(t.getIndex(), t.getNewValue()));
        });
    }

    private void setupListView(ListView<Task> list) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of ListCell
        list.setCellFactory(new Callback<ListView<Task>, ListCell<Task>>() {
            @Override
            public LabelCell call(ListView<Task> param) {
                LabelCell labelCell = new LabelCell() {};

                labelCell.setOnDragDetected( (MouseEvent event) -> {
                    if (!labelCell.getItem().getText().equals("")) {
                        Dragboard db = labelCell.startDragAndDrop(TransferMode.MOVE);
                        ClipboardContent content = new ClipboardContent();
                        content.put(dataFormat,labelCell.getItem());
                        db.setContent(content);
                    }
                    event.consume();
                });

                labelCell.setOnDragOver(event -> {
                    if (event.getGestureSource() != labelCell && event.getDragboard().hasContent(dataFormat)) {
                        event.acceptTransferModes(TransferMode.MOVE);
                    }
                    event.consume();
                });

                labelCell.setOnDragEntered(event -> {
                    if (event.getGestureSource() != labelCell && event.getDragboard().hasContent(dataFormat)) {
                        System.out.println("TODO: change color of listview"); //todo
                    }
                    event.consume();
                });

                labelCell.setOnDragExited(event -> {
                    System.out.println("TODO reset color of listview"); //todo
                    event.consume();
                });

                labelCell.setOnDragDropped(event -> {
                    Dragboard db = event.getDragboard();
                    boolean success = false;
                    if (db.hasContent(dataFormat)) {
                        Task newTask = (Task) db.getContent(dataFormat);
                        //insert new task, removing will happen in onDragDone
                        int index = min(labelCell.getIndex(),list.getItems().size()); // item can be dropped way below the existing list
                        //we have put an empty item instead of no items
                        //because otherwise there are no listCells that can receive an item
                        if (list.getItems().get(index).getText().equals("")) {
                            list.getItems().set(index,newTask); //replace empty item
                        } else {
                            list.getItems().add(index, newTask);
                        }
                        success = true;
                    }
                    event.setDropCompleted(success);
                    event.consume();
                });

                labelCell.setOnDragDone(event -> {
                    //ensures the original element is only removed on a valid copy transfer (no dropping outside listviews)
                    if (event.getTransferMode() == TransferMode.MOVE) {
                        Dragboard db = event.getDragboard();
                        Task newTask = (Task) db.getContent(dataFormat);
                        Task emptyTask = new Task("","");
                        //remove original item
                        //item can have been moved up (so index becomes one too much)
                        // or such that the index didn't change, like to another day
                        if (list.getItems().get(labelCell.getIndex()).getText().equals(newTask.getText())) {
                            list.getItems().set(labelCell.getIndex(),emptyTask);
                        } else {
                            list.getItems().set(labelCell.getIndex()+1,emptyTask);
                        }
                        //prevent an empty list from refusing to receive items, as it wouldn't contain any listcell
                        if (list.getItems().size() < 1) {
                            list.getItems().add(emptyTask);
                        }
                    }
                    event.consume();
                });

                return labelCell;
            }
        });
    }

    static class LabelCell extends TextFieldListCell<Task> {
        HBox hbox = new HBox();
        TextField textField = new TextField("(empty)");
        Pane pane = new Pane();
        ObservableList<String> comboList = FXCollections.observableArrayList(
                "0LAUK0","2WF50","2WA70","2IPC0");
        ComboBox<String> comboBox = new ComboBox<>(comboList);

        public LabelCell() {
            super();
            hbox.getChildren().addAll(textField, pane, comboBox);
            HBox.setHgrow(pane, Priority.ALWAYS);

            textField.setOnAction(e -> {
                Task newItem = getItem();
                newItem.setText(textField.getText());
                newItem.setLabel(comboBox.getValue());
                commitEdit(newItem);
            });

        }

        @Override
        public void updateItem(Task task, boolean empty) {
            super.updateItem(task, empty);
            if (empty || task == null) {
                setGraphic(null);
                setText(null); // No text in textField of super class
            } else if (isEditing()) {
                textField.setText(task.getText());
                setText(null);
                setGraphic(hbox);
            } else {
                setText(task.getText());
                setGraphic(null);
            }
        }

        @Override
        public void startEdit() {
            super.startEdit();
            textField.setText(getItem().getText());
            setText(null);
            setGraphic(hbox);
            textField.selectAll();
            textField.requestFocus();
        }

        @Override
        public void cancelEdit() {
            super.cancelEdit();
            setText(getItem().getText());
            setGraphic(null);
        }

        @Override
        public void commitEdit(Task task) {
            super.commitEdit(task);
            getListView().getItems().set(getIndex(), task);
            setText(getItem().getText());
            setGraphic(null);
        }
    }
    //when transferred with the dragboard, the object is serialized
    //which I think means that a new object is created and you lose the reference to the old one
    //which I think should be fine here, as only content matters
    static class Task implements Serializable {
        private String text;
        private String label;

        public Task(String text, String label) {
            this.text = text;
            this.label = label;
        }

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }

}
