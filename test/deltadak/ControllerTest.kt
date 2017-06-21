package deltadak

import javafx.collections.FXCollections
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Assertions.fail
import java.awt.Color


object ControllerTest: Spek({
    given("A Controller") {
        val controller = Controller()
        on("converting and ObservableList to an ArrayList") {
            val listToConvert = ArrayList<HomeworkTask>()
            val task1 = HomeworkTask()
            val task2 = HomeworkTask()
            listToConvert.add(task1)
            listToConvert.add(task2)
            val observableList = FXCollections.observableArrayList<HomeworkTask>(listToConvert)
            val convertedList = controller.convertObservableListToList(observableList)
            it("should keep the same size, and the same elements") {
                assertTrue(convertedList.size == 2, "size should be the same after converting")
                assertTrue(convertedList.containsAll(listToConvert), "contents should be the same after converting")
            }
        }

        on("converting a color in text to hexadecimal") {
            val hex = controller.convertColorToHex("Green")
            it("should return valid hexadecimal number") {
                try {
                    Color.decode(hex)
                } catch (e: NumberFormatException) {
                    fail(hex + " is not a valid hex color")
                }

            }
        }
    }
})
