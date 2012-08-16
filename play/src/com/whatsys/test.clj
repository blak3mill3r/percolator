(ns com.whatsys.test
  ( :use     [percolator.core]))

(definterpreter interpret-async-rpc [service parameter-list success failure]
  `( '. greetingService greetServer textToServer
     ('new AsyncCallback<String> 
       ( 'method #{:public} void onSuccess ~parameter-list ~@success)
       ( 'method #{:public} void onFailure [(Throwable e)] ~@failure))))

(definterpreter interpret-statement-on-click [identifier-symbol & statements]
  `( '. ~identifier-symbol addClickHandler 
     ('new ClickHandler
       ( 'method #{:public} void onClick [ (ClickEvent e) ] ~@statements ))))

(add-body-decl-interpreters
  { '(quote on-click)   (interpreter [& statements] `( 'method #{:public} void onClick [ (ClickEvent e) ] ~@statements ))
    '(quote on-key-up)  (interpreter [& statements] `( 'method #{:public} void onKeyUp [ (KeyUpEvent e) ] ~@statements ))
   })

(add-statement-interpreters
  { '(quote disable)    (interpreter [o]   `( '. ~o setEnabled false      ))
    '(quote enable)     (interpreter [o]   `( '. ~o setEnabled true       ))
    '(quote select-all) (interpreter [o]   `( '. ~o selectAll             ))
    '(quote hide)       (interpreter [o]   `( '. ~o hide                  ))
    '(quote show)       (interpreter [o]   `( '. ~o show                  ))
    '(quote focus)      (interpreter [o]   `( '. ~o setFocus true         ))
    '(quote unfocus)    (interpreter [o]   `( '. ~o setFocus false        ))
    '(quote center)     (interpreter [o]   `( '. ~o center                ))
    '(quote set-widget) (interpreter [o s] `( '. ~o setWidget       ~s    ))
    '(quote add)        (interpreter [o s] `( '. ~o add             ~s    ))
    '(quote set-text)   (interpreter [o s] `( '. ~o setText         ~s    ))
    '(quote style)      (interpreter [o s] `( '. ~o addStyleName    ~s    ))
    '(quote unstyle)    (interpreter [o s] `( '. ~o removeStyleName ~s    ))
    '(quote html=)      (interpreter [o s] `( '. ~o setHTML         ~s    ))
    '(quote on-click)   interpret-statement-on-click
    '(quote async)      interpret-async-rpc
    '(quote button)     (interpreter [n] `( 'local #{:final} Button    (~n ('new Button      )) ))
    '(quote dialog-box) (interpreter [n] `( 'local #{:final} DialogBox (~n ('new DialogBox   )) ))
    })

(definterpreter interpret-expression-gwt-new [class-name]
  `( '. GWT create ( 'class-expr ~class-name ) ))

(add-expression-interpreters
  { '(quote gwt-new) interpret-expression-gwt-new })


; that was for gwt
; for an app, could extend further with shorthands
; which are kinda like inlined methods
; they're expanded at compile time

(definterpreter interpret-show-dialog [s]
  `( 'block
     ( 'html= serverResponseLabel ~s )
     ( 'center dialogBox )
     ( 'focus closeButton )
     ))

(add-statement-interpreters
  { '(quote show-dialog) interpret-show-dialog
   })

(wrap-a-class-kluge
  com.whatsys.test.client
  [ com.whatsys.test.shared.FieldVerifier
    com.google.gwt.core.client.EntryPoint
    com.google.gwt.core.client.GWT
    com.google.gwt.event.dom.client.ClickEvent
    com.google.gwt.event.dom.client.ClickHandler
    com.google.gwt.event.dom.client.KeyCodes
    com.google.gwt.event.dom.client.KeyUpEvent
    com.google.gwt.event.dom.client.KeyUpHandler
    com.google.gwt.user.client.rpc.AsyncCallback
    com.google.gwt.user.client.ui.Button
    com.google.gwt.user.client.ui.DialogBox
    com.google.gwt.user.client.ui.HTML
    com.google.gwt.user.client.ui.Label
    com.google.gwt.user.client.ui.RootPanel
    com.google.gwt.user.client.ui.TextBox
    com.google.gwt.user.client.ui.VerticalPanel ]

  (class-decl

  #{:public} Test

    ( 'implements EntryPoint )

    ( 'field #{:private :static :final} String (SERVER_ERROR "D'oh!") )

    ( 'field #{:private :final} GreetingServiceAsync (greetingService ( 'gwt-new GreetingService )) )

    ( 'method #{:public} void onModuleLoad []
      ( 'local #{:final} Button  (sendButton ('new Button "Send")) )
      ( 'local #{:final} TextBox (nameField ('new TextBox)) )
      ( '. nameField setText "GWT User" )
      ( 'local #{:final} Label (errorLabel ('new Label)) )
      ( '. sendButton addStyleName "sendButton" )

      ( 'add  ( '. RootPanel get "nameFieldContainer"  ) nameField )
      ( 'add  ( '. RootPanel get "sendButtonContainer" ) sendButton )
      ( 'add  ( '. RootPanel get "errorLabelContainer" ) errorLabel )

      ;( 'add ( '>RootPanel/nameFieldContainer ) nameField )
        ; cool idea
        ; if a symbol starts with a special character like > above
        ; it is a special gwt-specific expression
        ; such as apanel.get("a_domid")

      ( 'focus nameField )
      ( 'select-all nameField )

      ; would be nice if above could be written
      ;( 'doto nameField
      ;  ('focus)
      ;  ('select-all))

      ( 'local #{:final} DialogBox (dialogBox ('new DialogBox)) )
      ( 'dialog-box dialogBox )

      ( '. dialogBox setText "RPC" )
      ( '. dialogBox setAnimationEnabled true )

      ( 'local #{:final} Button ( closeButton ('new Button "Close"))  )
      ( 'button closeButton )

      ( '. ( '. closeButton getElement ) setId "closeButton" )

      ( 'local #{:final} Label ( textToServerLabel ( 'new Label )) )
      ( 'local #{:final} HTML ( serverResponseLabel ( 'new HTML )) )

        ;; this whole bit is the part to rewrite
      ( 'local #{} VerticalPanel ( dialogVPanel ( 'new VerticalPanel )) )

      ( 'style dialogVPanel "dialogVPanel" )
      ( 'add dialogVPanel ( 'new HTML "<b> Sending name to the server: </b>" ) )
      ( 'add dialogVPanel textToServerLabel )
      ( 'add dialogVPanel ( 'new HTML "<br><b> Server replies: </b>" ) )
      ( 'add dialogVPanel serverResponseLabel )
      ( '. dialogVPanel setHorizontalAlignment VerticalPanel/ALIGN_RIGHT )

      ( 'set-widget dialogBox dialogVPanel )
        ; ending here...

      ; rewrite above as
      ;   (comment

      ;( 'vpanel dialogVPanel )

           ; a series of panel manipulations
           ; a new percolator "scope"
           ; the existing scopes being body-decl statement and expression
           ; so make this work, and then see if the commonalities in the
           ; definitions of the different scopes can be abstracted away
           ; so '<-- would be a statement interpreter
           ; producing a BlockStmt
           ; kinda like a doto but the forms are panel manipulations
      ;( '<-- dialogVPanel
      ;  :.dialogVPanel             ; keyword starting with . css style name
      ;  :#foo                      ; keyword starting with # dom id
      ;  "<b>Sending name etc </b>" ; straight HTML
      ;  textToServerLabel          ; symbol not beginning with . is a widget to add
      ;  [ :b "Server Response" ]   ; hiccup, would be insanely cool to support run-time variables in here
      ;  serverResponseLabel        
      ;  :align-right               ; keyword are special syntax that map to special things like setHorizontalAlignment VerticalPanel/ALIGN_RIGHT
      ;)

      ;     )

      ( 'on-click closeButton
        ( 'hide dialogBox )
        ( 'enable sendButton )
        ( 'focus sendButton ))

      ( 'class #{} MyHandler
        ( 'implements ClickHandler KeyUpHandler ) ; TODO it would be cool if this could be inferred from the use of 'on-click etc as body-decls, first need to make 'implements merge

        ( 'on-click ( '. this sendNameToServer ))

        ( 'on-key-up
          ( 'if ( '==
                  ( '. e getNativeKeyCode )
                  KeyCodes/KEY_ENTER )
            (('. this sendNameToServer ))))

        ( 'method #{:public} void sendNameToServer []
          ( 'set-text errorLabel "" )

          ( 'local #{} String (textToServer ('. nameField getText)) )

          ( 'if ('! ('. FieldVerifier isValidName textToServer))
              ( ('. errorLabel setText "Please do the thing right")
                ( 'return )))

          ( 'set-text textToServerLabel textToServer )
          ( 'set-text serverResponseLabel "" )

         ( 'async greeting/greet [(String result)]
           (( 'set-text dialogBox "RPC Success Sauce (hax)" )
            ( 'unstyle serverResponseLabel "serverResponseLabelError" )
            ( 'show-dialog result ))
           (( 'set-text dialogBox "megaFAIL" )
            ( 'style serverResponseLabel "serverResponseLabelError" )
            ( 'show-dialog SERVER_ERROR )))
            )
        ); end class MyHandler

      ( 'local #{} MyHandler ( handler ('new MyHandler )))
      ( '. sendButton addClickHandler handler )
      ( '. nameField addKeyUpHandler handler )

  )))
