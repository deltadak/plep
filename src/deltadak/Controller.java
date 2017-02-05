package deltadak;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
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
import javafx.util.StringConverter;
import javafx.util.converter.DefaultStringConverter;

import java.io.Serializable;
import java.net.URL;
import java.util.ResourceBundle;

import static java.lang.Integer.min;

public class Controller implements Initializable {
    @FXML ListView<Task> day1;
    @FXML ListView<Task> day2;
    @FXML GridPane gridpane;
    private DataFormat dataFormat = new DataFormat("com.deltadak.Task");

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
                new Task("gdv","2WA70"),
                new Task("methods","2IPC0")
        );
        ObservableList<Task> day2Tasks = FXCollections.observableArrayList(
                new Task("discrete","2WF50"),
                new Task("robot","0LAUK0")
        );
        day1.setItems(day1Tasks);
        day2.setItems(day2Tasks);

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

                //update text on changes
                labelCell.setConverter(new TaskConverter(labelCell));

                // update label on changes
                labelCell.comboBox.valueProperty().addListener(new ChangeListener<String>() {
                    @Override
                    public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
                        labelCell.getItem().setLabel(newValue);
                    }
                });

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
                    cleanUp(list);
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
                            labelCell.setGraphic(null);
                        } else {
                            list.getItems().set(labelCell.getIndex() + 1, emptyTask);
                        }
                        //prevent an empty list from refusing to receive items, as it wouldn't contain any listcell
                        if (list.getItems().size() < 1) {
                            list.getItems().add(emptyTask);
                        }
                    }
                    event.consume();
                    cleanUp(list);
                });

                return labelCell;
            }
        });
        cleanUp(list);
    }

    /**
     * removes empty rows, and then fills up with empty rows
     * @param list to clean up
     */
    private void cleanUp(ListView<Task> list) {
        int maxListLength = 6; //todo variable listview length
        int i;
        //first remove empty items
        for (i=0; i < list.getItems().size(); i++) {
            if (list.getItems().get(i).getText().equals("")) {
                list.getItems().remove(i);
            }
        }
        //fill up if necessary
        for (i = 0; i<maxListLength; i++) {
            if (i >= list.getItems().size()) {
                list.getItems().add(i, new Task("",""));
            }
        }
    }

    private class LabelCell extends TextFieldListCell<Task> {
        HBox hbox = new HBox();
        Label text = new Label("");
        Pane pane = new Pane();
        ObservableList<String> comboList = FXCollections.observableArrayList(
                "0LAUK0","2WF50","2WA70","2IPC0");
        ComboBox<String> comboBox = new ComboBox<>(comboList);

        private LabelCell() {
            super();
            hbox.getChildren().addAll(text, pane, comboBox);
            HBox.setHgrow(pane, Priority.ALWAYS);
        }

        /**
         * called when starting edit with (null, true)
         * and when finished edit with (task, false)
         * @param task
         * @param empty
         */
        @Override
        public void updateItem(Task task, boolean empty) {
            super.updateItem(task, empty);
            setText(null);
            if (empty) {
                setGraphic(null);
            } else {
                text.setText(task.getText() != null ? task.getText() : "<null>");
                comboBox.setValue(task.getLabel() != null ? task.getLabel() : "<null>");
                setGraphic(hbox);
            }
        }
    }

    /**
     * custom stringconverter to define what editing a listcell means
     * this converter is set on each listcell
     */
    private class TaskConverter extends StringConverter<Task> {
        private final ListCell<Task> cell;
        private TaskConverter(ListCell<Task> cell) {
            this.cell = cell;
        }
        @Override
        public String toString(Task task) {
            return task.getText();
        }

        @Override
        public Task fromString(String string) {
            Task task = cell.getItem();
            task.setText(string);
            return task;
        }
    }


    //when transferred with the dragboard, the object is serialized
    //which I think means that a new object is created and you lose the reference to the old one
    //which I think should be fine here, as only content matters
    static class Task implements Serializable {
        private String text;
        private String label;

        private Task(String text, String label) {
            this.text = text;
            this.label = label;
        }

        private String getText() {
            return text;
        }

        private void setText(String text) {
            this.text = text;
        }

        private String getLabel() {
            return label;
        }

        private void setLabel(String label) {
            this.label = label;
        }
    }

}
