package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.junit.jupiter.api.Test;

import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test some random Controller methods.
 */
class ControllerTest {
    @Test
    void convertObservableToArrayList() {
        Controller controller = new Controller();
        Collection<HomeworkTask> listToConvert = new ArrayList<>();
        HomeworkTask task1 = new HomeworkTask("asdf", "fda", "colooor");
        HomeworkTask task2 = new HomeworkTask("bladf", "fda", "colooor");
        listToConvert.add(task1);
        listToConvert.add(task2);
        ObservableList<HomeworkTask> observableList = FXCollections.observableArrayList(listToConvert);
        List<HomeworkTask> convertedList = controller.convertObservableListToList(observableList);
        assertTrue(convertedList.size() == 2, "size should be the same after converting");
        assertTrue(convertedList.containsAll(listToConvert), "contents should be the same after converting");
    }

    @Test
    void convertColorToHex() {
        fail("testing travis");
        Controller controller = new Controller();
        String hex = controller.convertColorToHex("Green");
        try {
            Color.decode(hex);
        } catch (NumberFormatException e) {
            fail(hex + " is not a valid hex color");
        }
    }

}