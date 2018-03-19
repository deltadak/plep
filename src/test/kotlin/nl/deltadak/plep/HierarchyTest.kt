package nl.deltadak.plep

import javafx.scene.control.Button
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane
import nl.deltadak.plep.ui.settingspane.panes.isInHierarchy
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue

object HierarchyTest: Spek({
    given("a hierarchy of objects, and a node") {
        val bottom = Button()

        val intermediate = GridPane()
        intermediate.children.add(bottom)

        val topNode = AnchorPane()
        topNode.children.add(intermediate)

        on("checking if the bottom is in the hierarchy") {
            val inHierarchy = isInHierarchy(bottom, topNode)
            it("should have returned true") {
                assertTrue(inHierarchy)
            }
        }

        on("checking that a new object is not in the hierarchy") {
            val inHierarchy = isInHierarchy(Button(), topNode)
            it("should have returned false") {
                assertFalse(inHierarchy)
            }
        }

        on("checking that a new top object is not in the hierarchy") {
            val inHierarchy = isInHierarchy(bottom, AnchorPane())
            it("should have returned false") {
                assertFalse(inHierarchy)
            }
        }

    }
})
