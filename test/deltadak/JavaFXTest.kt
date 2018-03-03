package deltadak

import javafx.scene.control.ProgressIndicator
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import org.testfx.api.FxToolkit

object JavaFXTest: Spek({
    given("the JavaFX Toolkit") {
        // Initialise JavaFX Toolkit, needed for things like ProgressIndicator.
        FxToolkit.registerPrimaryStage()
        FxToolkit.setupApplication(Main::class.java)

        on("instantiating a JavaFX component") {
            val progress = ProgressIndicator()
            it("should not throw any errors") {
                progress.isVisible = false
            }
        }

    }
})
