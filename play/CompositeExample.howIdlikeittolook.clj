('composite OptionalTextBox [(String caption)]
   ('vertical-panel panel
     ('check-box checkBox [] ; no ctor arguments given
        ( setText caption ) ; kinda like doto syntax
        ( setChecked true ) ; shorthand for these things?
        :on-click this      ; adds this (the OptionalTextBox instance) as the ClickHandler, which also makes the OptionalTextBox class implement ClickHandler
        ) 
     ('text-box textBox)   ; no ctor arguments given
      :.example-OptionalCheckBox ; style name for composite
      :click
        (
          ( 'if ( '== sender checkBox )
              ( ( '. textBox setEnabled
                  ( '. checkBox isChecked ) ) ))
        )
      :expose
        (
          ( checkBox text caption ) ; figures out setCaption and getCaption (this will require introspection of the CheckBox class to find out what getText returns .. thoroughly badass though)
        ) 
      ) ; since this vertical panel is the outermost form
        ; it should figure out it needs to initWidget(panel)

   )
