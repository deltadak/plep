package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.GridPane;
import javafx.util.Callback;
import javafx.util.converter.DefaultStringConverter;

import java.net.URL;
import java.util.ResourceBundle;

import static java.lang.Integer.min;

public class Controller implements Initializable {
    @FXML ListView<String> day1;
    @FXML ListView<String> day2;
    @FXML GridPane gridpane;

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
        ObservableList<String> day1List = FXCollections.observableArrayList("task1","task2","","","","","","");
        ObservableList<String> day2List = FXCollections.observableArrayList("task3","task4","","","","","","");
        day1.setItems(day1List);
        day2.setItems(day2List);

        //setup drag and drop for all children of gridview
        gridpane.getChildren().stream().filter(node -> node instanceof ListView).forEach(node -> {
            ListView<String> list = (ListView<String>) node;
            setupListView(list);
            list.setOnEditCommit(t -> list.getItems().set(t.getIndex(), t.getNewValue()));
        });
    }

    private void setupListView(ListView<String> list) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of ListCell
        list.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
            @Override
            public TextFieldListCell<String> call(ListView<String> param) {
                TextFieldListCell<String> listCell = new TextFieldListCell<String>() {
                    @Override
                    public void updateItem( String item, boolean empty) {
                        super.updateItem(item, empty);
                        setText(item);
                    }
                };

                //set converter to convert text input into object and back when editing
                listCell.setConverter(new DefaultStringConverter());

                listCell.setOnDragDetected( (MouseEvent event) -> {
                    if (!listCell.getItem().equals("")) {
                        Dragboard db = listCell.startDragAndDrop(TransferMode.COPY);
                        ClipboardContent content = new ClipboardContent();
                        content.putString(listCell.getItem());
                        db.setContent(content);
                    }
                    event.consume();
                });

                listCell.setOnDragOver(event -> {
                    if (event.getGestureSource() != listCell && event.getDragboard().hasString()) {
                        event.acceptTransferModes(TransferMode.COPY_OR_MOVE);
                    }
                    event.consume();
                });

                listCell.setOnDragEntered(event -> {
                    if (event.getGestureSource() != listCell && event.getDragboard().hasString()) {
                        System.out.println("TODO: change color of listview"); //todo
                    }
                    event.consume();
                });

                listCell.setOnDragExited(event -> {
                    System.out.println("TODO reset color of listview"); //todo
                    event.consume();
                });

                listCell.setOnDragDropped(event -> {
                    Dragboard db = event.getDragboard();
                    boolean success = false;
                    if (db.hasString()) {
                        String newvalue = db.getString();
                        //insert new task, removing will happen in onDragDone
                        int index = min(listCell.getIndex(),list.getItems().size()); // item can be dropped way below the existing list
                        //we have put an empty item instead of no items
                        //because otherwise there are no listCells that can receive an item
                        if (list.getItems().get(index).equals("")) {
                            list.getItems().set(index,newvalue); //replace empty item
                        } else {
                            list.getItems().add(index, newvalue);
                        }
                        success = true;
                    }
                    event.setDropCompleted(success);
                    event.consume();
                });

                listCell.setOnDragDone(event -> {
                    //ensures the original element is only removed on a valid copy transfer (no dropping outside listviews)
                    if (event.getTransferMode() == TransferMode.COPY) {
                        Dragboard db = event.getDragboard();
                        String draggedvalue = db.getString();
                        //remove original item
                        //item can have been moved up (so index becomes one too much)
                        // or such that the index didn't change, like to another day
                        if (list.getItems().get(listCell.getIndex()).equals(draggedvalue)) {
                            list.getItems().set(listCell.getIndex(),"");
                        } else {
                            list.getItems().set(listCell.getIndex()+1,"");
                        }
                        //prevent an empty list from refusing to receive items, as it wouldn't contain any listcell
                        if (list.getItems().size() < 1) {
                            list.getItems().add("");
                        }
                    }
                    event.consume();
                });

                return listCell;
            }
        });
    }


}
