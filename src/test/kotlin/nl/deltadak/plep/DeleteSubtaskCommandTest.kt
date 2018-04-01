package nl.deltadak.plep

import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeItem
import javafx.scene.control.TreeView
import nl.deltadak.plep.commands.DeleteSubtaskCommand
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import org.testfx.api.FxToolkit
import java.time.LocalDate
import kotlin.test.assertFalse
import kotlin.test.assertTrue

object DeleteSubtaskCommandTest: Spek({
    given("A command to delete a subtask") {
        // Initialise JavaFX Toolkit, needed for things like ProgressIndicator.
        FxToolkit.registerPrimaryStage()
        FxToolkit.setupApplication(Main::class.java)

        val day = LocalDate.now()
        val index = 1
        val subtask = HomeworkTask()
        subtask.text = "subtask"
        val treeViewItems = mutableListOf<MutableList<HomeworkTask>>()
        treeViewItems.add(mutableListOf(HomeworkTask(), subtask))

        // It doesn't really matter what's in here, but it needs something.
        val tree = TreeView<HomeworkTask>()
        tree.root = TreeItem<HomeworkTask>().apply { children.add(TreeItem<HomeworkTask>(HomeworkTask())) }

        val command = DeleteSubtaskCommand(ProgressIndicator(), day, treeViewItems,index, tree)
        on("executing the command") {
            command.execute()
            it("should be executed") {
                assertTrue(command.isExecuted)
            }
        }
        on("undoing the command") {
            if (!command.isExecuted) {
                command.execute()
            }
            command.undo()
            it("should not be executed") {
                assertFalse { command.isExecuted }
            }
            it("should have only one parent") {
                assertTrue { command.treeViewItems.size == 1 }
            }
            it("should have two children") {
                assertTrue { command.treeViewItems.first().size == 2 }
            }
            it("should be an empty child") {
                assertTrue { command.treeViewItems.first().first().text == "" }
            }
        }
        on("deleting a subtask") {
            command.execute()
            it("should have deleted the subtask") {
                assertTrue { command.treeViewItems.first().size == 1 }
            }
            it("should have empty parent text") {
                assertTrue { command.treeViewItems.first().first().text == "" }
            }
        }
    }
})
