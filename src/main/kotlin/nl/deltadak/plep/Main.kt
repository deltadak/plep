package nl.deltadak.plep

import com.sun.jna.Native
import com.sun.jna.NativeLong
import com.sun.jna.WString
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.Parent
import javafx.scene.Scene
import javafx.scene.image.Image
import javafx.stage.Screen
import javafx.stage.Stage
import nl.deltadak.plep.ui.Controller


/**
 * Main class.
 */
class Main : Application() {

    /**
     * {@inheritdoc}
     */
    override fun start(primaryStage: Stage) {
        registerTaskbar()

        val loader = FXMLLoader(javaClass.getResource("/interface.fxml"))
        val root: Parent = loader.load()

        // Used to invoke a setup method in the Controller which needs the stage.
        val controller = loader.getController<Controller>()
        DayChangeListener(controller).setup(primaryStage)


        with(primaryStage) {
            title = "Plep"
            scene = Scene(root, 0.0, 0.0)

            // Set a size relative to the screen.
            val primaryScreenBounds = Screen.getPrimary().visualBounds
            // Set the Stage boundaries to the visible bounds of the main screen.
            x = primaryScreenBounds.minX
            y = primaryScreenBounds.minY
            // Default startup behaviour is half-screen.
            width = primaryScreenBounds.width / 2
            height = primaryScreenBounds.height
        }

        val css = arrayOf(
                "/css/general.css",
                "/css/button.css",
                "/css/treeview/treeview.css",
                "/css/treeview/checkbox.css",
                "/css/treeview/dropdown.css",
                "/css/treeview/scrollbar.css"
        )

        css.map {
            this@Main.javaClass.getResource(it).toExternalForm()
        }.forEach {
            primaryStage.scene.stylesheets.add(it)
        }

        // Check if running in debug mode,
        // to display the default java icon so we can distinguish between
        // the program we are testing and the one we are actually using
        // (the latter has the plep logo).
        val isDebug = java.lang
                .management
                .ManagementFactory
                .getRuntimeMXBean()
                .inputArguments
                .toString()
                .indexOf("-agentlib:jdwp") > 0
        if (!isDebug) {
            primaryStage.icons.add(Image(this.javaClass.getResourceAsStream("/icon.png")))
        }

        primaryStage.show()
    }

}

/**
 *  Main method.
 * @param args Startup args.
 */
fun main(args: Array<String>) {
    Application.launch(Main::class.java, *args)
}

/**
 * Register Plep to Windows, specifically the App User Model ID.
 * Seems to need be outside of the Main class.
 */
fun registerTaskbar() {
    Native.register("shell32")
    setCurrentProcessExplicitAppUserModelID(Main::class.java.name)
}

/**
 * Make the app pinnable to the Windows taskbar.
 */
fun setCurrentProcessExplicitAppUserModelID(appID: String) {
    if (SetCurrentProcessExplicitAppUserModelID(WString(appID)) != NativeLong(0))
        throw RuntimeException("unable to set current process explicit AppUserModelID to: $appID")
}

@Suppress("FunctionName") // C!
private external fun SetCurrentProcessExplicitAppUserModelID(appID: WString): NativeLong
