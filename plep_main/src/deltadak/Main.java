package deltadak;

import deltadak.ui.Controller;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Rectangle2D;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Screen;
import javafx.stage.Stage;

/**
 * main class
 */
public class Main extends Application {

    @Override
    public void start(final Stage primaryStage) throws Exception{
        FXMLLoader loader = new FXMLLoader(getClass().getResource("/interface.fxml"));
        Parent root = loader.load();
        
        //used to invoke a setup method in controller which needs the stage
        Controller controller = loader.getController();
        controller.setDayChangeListener(primaryStage);
        
        primaryStage.setTitle("Plep");
        primaryStage.setScene(new Scene(root, 0, 0));
    
        //set a size relative to screen
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        //set Stage boundaries to visible bounds of the main screen
        primaryStage.setX(primaryScreenBounds.getMinX());
        primaryStage.setY(primaryScreenBounds.getMinY());
        primaryStage.setWidth(primaryScreenBounds.getWidth()/2);
        primaryStage.setHeight(primaryScreenBounds.getHeight());
        
        String listViewCSS = this.getClass().getResource("/css/listview.css").toExternalForm();
        String generalCSS = this.getClass().getResource("/css/general.css").toExternalForm();
        primaryStage.getScene().getStylesheets().addAll(listViewCSS, generalCSS);
    
        // check if running in debug mode
        // to display the default java icon so we can distinguish between
        // the program we are testing and the one we are actually using
        // (the latter has the plep logo)
        boolean isDebug = java.lang.management.ManagementFactory.getRuntimeMXBean().
                getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
        if (!isDebug) {
            primaryStage.getIcons().add(new Image(this.getClass().getResourceAsStream("/icon.png")));
        }

        primaryStage.show();

    }
    
    /**
     *  main method
     * @param args args
     */
    public static void main(final String[] args) {
        launch(args);
    }
}
