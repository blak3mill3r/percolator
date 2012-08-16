public class CompositeExample implements EntryPoint {

  /**
   * A composite of a TextBox and a CheckBox that optionally enables it.
   */
  private static class OptionalTextBox extends Composite implements
      ClickListener {

    private TextBox textBox = new TextBox();
    private CheckBox checkBox = new CheckBox();

    /**
     * Constructs an OptionalTextBox with the given caption on the check.
     * 
     * @param caption the caption to be displayed with the check box
     */
    public OptionalTextBox(String caption) {
      // Place the check above the text box using a vertical panel.
      VerticalPanel panel = new VerticalPanel();
      panel.add(checkBox);
      panel.add(textBox);

      // Set the check box's caption, and check it by default.
      checkBox.setText(caption);
      checkBox.setChecked(true);
      checkBox.addClickListener(this);

      // All composites must call initWidget() in their constructors.
      initWidget(panel);

      // Give the overall composite a style name.
      setStyleName("example-OptionalCheckBox");
    }

    public void onClick(Widget sender) {
      if (sender == checkBox) {
        // When the check box is clicked, update the text box's enabled state.
        textBox.setEnabled(checkBox.isChecked());
      }
    }

    /**
     * Sets the caption associated with the check box.
     * 
     * @param caption the check box's caption
     */
    public void setCaption(String caption) {
      // Note how we use the use composition of the contained widgets to provide
      // only the methods that we want to.
      checkBox.setText(caption);
    }

    /**
     * Gets the caption associated with the check box.
     * 
     * @return the check box's caption
     */
    public String getCaption() {
      return checkBox.getText();
    }
  }

  public void onModuleLoad() {
    // Create an optional text box and add it to the root panel.
    OptionalTextBox otb = new OptionalTextBox("Check this to enable me");
    RootPanel.get().add(otb);
  }
}

