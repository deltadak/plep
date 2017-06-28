package deltadak

import org.junit.jupiter.api.Assertions.assertFalse
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import org.junit.jupiter.api.Assertions.assertTrue
import java.time.LocalDate

object EditTextCommandTest: Spek({
    given("A command to edit text") {
        val day = LocalDate.now()
        val list = ArrayList<HomeworkTask>()
        list.add(HomeworkTask())
        val command = EditTextCommand(day, list)

        on("executing an EditTextCommand") {
            it("should not be executed initially") {
                assertFalse(command.isExecuted)
            }
            it("should be executed after execution") {
                command.execute()
                assertTrue(command.isExecuted)
            }
            it("should not be executed after undo") {
                command.undo()
                assertFalse(command.isExecuted)
            }
        }

    }
})
