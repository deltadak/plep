package deltadak;

import java.io.Serializable;

/** Serializable: When transferred with the dragboard, the object is serialized
which I think means that a new object is created and you lose the
reference to the old one which I think should be fine here, as only content matters */
public class HomeworkTask implements Serializable {
    
    private boolean done;
    private String text;
    private String label;
    private int colorID;
    private boolean expanded;
    private int databaseID; // The id (primary key) in the database, used for
    // the subtasks to link to their parent. For the subtasks, this id is -1.
    
    public HomeworkTask(final boolean done, final String text, final String
            label, final int colorID, final boolean expanded, final int
            databaseID) {
        this.done = done;
        this.text = text;
        this.label = label;
        this.colorID = colorID;
        this.expanded = expanded;
        this.databaseID = databaseID;
    }
    
    /**
     * Subtasks constructor.
     * @param done
     * @param text
     */
    public HomeworkTask(final boolean done, final String text) {
        this.done = done;
        this.text = text;
        this.label = "";
        this.colorID = 4;
        this.expanded = false;
        this.databaseID = -1;
    }

    /**
     * Default task.
     */
    public HomeworkTask() {
        this.done = false;
        this.text = "";
        this.label = "";
        this.colorID = 4;
        this.expanded = false;
        this.databaseID = -1;
    }

    public boolean getDone() {
        return done;
    }

    public void setDone(final boolean done) {
        this.done = done;
    }
    
    public String getText() {
        return text;
    }
    
    public void setText(final String text) {
        this.text = text;
    }
    
    public String getLabel() {
        return label;
    }
    
    public void setLabel(final String label) {
        this.label = label;
    }
    
    public int getColorID() {
        return colorID;
    }
    
    public void setColorID(final int colorID) {
        this.colorID = colorID;
    }
    
    public boolean getExpanded() {
        return expanded;
    }
    
    public void setExpanded(final boolean expanded) {
        this.expanded = expanded;
    }

    public int getDatabaseID() {
        return databaseID;
    }

    public void setDatabaseID(int databaseID) {
        this.databaseID = databaseID;
    }
}