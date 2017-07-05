package deltadak;

import java.io.Serializable;

//when transferred with the dragboard, the object is serialized
//which I think means that a new object is created and you lose the
// reference to the old one
//which I think should be fine here, as only content matters
class HomeworkTask implements Serializable {
    
    private boolean done;
    private String text;
    private String label;
    private String color;

    public HomeworkTask() {
        this.text = "default text";
        this.label = "default label";
        this.color = "white";
    }
    
    public HomeworkTask(final boolean done, final String text, final String label, final String color) {
        this.done = done;
        this.text = text;
        this.label = label;
        this.color = color;
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
    
    public String getColor() {
        return color;
    }
    
    public void setColor(final String color) {
        this.color = color;
    }
}
