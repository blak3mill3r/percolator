(ns com.whatsys.test
  ( :use     [percolator.core]))

(definterpreter async-rpc [service parameter-list success failure]
  `( '. greetingService greetServer textToServer
     ('new AsyncCallback<String> 
       ( 'method #{:public} void onSuccess ~parameter-list ~@success)
       ( 'method #{:public} void onFailure [(Throwable e)] ~@failure))))

(definterpreter on-click [identifier-symbol & statements]
  `( '. ~identifier-symbol addClickHandler 
     ('new ClickHandler
       ( 'method #{:public} void onClick [ (ClickEvent e) ] ~@statements ))))

(add-interpreters-to-scope :gwt-body-decl
  { 'on-click   (interpreter [& statements] `( 'method #{:public} void onClick [ (ClickEvent e) ] ~@statements ))
    'on-key-up  (interpreter [& statements] `( 'method #{:public} void onKeyUp [ (KeyUpEvent e) ] ~@statements ))
   })

(add-interpreters-to-scope :gwt-statement
  { 'disable    (interpreter [o]   `( '. ~o setEnabled false      ))
    'enable     (interpreter [o]   `( '. ~o setEnabled true       ))
    'select-all (interpreter [o]   `( '. ~o selectAll             ))
    'hide       (interpreter [o]   `( '. ~o hide                  ))
    'show       (interpreter [o]   `( '. ~o show                  ))
    'focus      (interpreter [o]   `( '. ~o setFocus true         ))
    'unfocus    (interpreter [o]   `( '. ~o setFocus false        ))
    'center     (interpreter [o]   `( '. ~o center                ))
    'set-widget (interpreter [o s] `( '. ~o setWidget       ~s    ))
    'add        (interpreter [o s] `( '. ~o add             ~s    ))
    'set-text   (interpreter [o s] `( '. ~o setText         ~s    ))
    'style      (interpreter [o s] `( '. ~o addStyleName    ~s    ))
    'unstyle    (interpreter [o s] `( '. ~o removeStyleName ~s    ))
    'html=      (interpreter [o s] `( '. ~o setHTML         ~s    ))
    'on-click   on-click
    'async      async-rpc
    'button     (interpreter [n] `( 'local #{:final} Button    (~n ('new Button      )) ))
    'dialog-box (interpreter [n] `( 'local #{:final} DialogBox (~n ('new DialogBox   )) ))
    'log        (interpreter [n] `( '. GWT log ~n ))
    })

(inherit-scope :statement :gwt-statement identity)
(inherit-scope :body-decl :gwt-body-decl identity)

(definterpreter gwt-new [class-name]
  `( '. GWT create ( 'class-expr ~class-name ) ))

(add-interpreters-to-scope :expression
  { 'gwt-new gwt-new })

; that was for gwt
; for an app, could extend further with shorthands
; which are kinda like inlined methods
; they're expanded at compile time

(definterpreter show-dialog [s]
  `( 'block
     ( 'html= serverResponseLabel ~s )
     ( 'center dialogBox )
     ( 'focus closeButton )
     ))

(add-interpreters-to-scope :statement
  { 'show-dialog show-dialog
   })

(compilation-unit
  ; package name
  com.whatsys.client
  ; imports
  [ com.whatsys.shared.FieldVerifier
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

  ; declare a public class Play
  (class-decl #{:public} Play

    ; which implements the EntryPoint interface
    ( 'implements EntryPoint )

    ; declare a static String field called SERVER_ERROR and initialize it
    ( 'field #{:private :static :final} String (SERVER_ERROR "D'oh!") )

    ; declare a member field of type GreetingServiceAsync and initialize it
    ( 'field #{:private :final} GreetingServiceAsync ( greetingService ('gwt-new GreetingService) ) )

    ; define a public, nullary method called onModuleLoad, returning void
    ( 'method #{:public} void onModuleLoad []
      ; declare and initialize local variables sendButton and nameField
      ( 'local #{:final} Button  (sendButton ('new Button "Send")) )
      ( 'local #{:final} TextBox (nameField ('new TextBox)) )
      ; call the method setText on the object nameField with the parameter "GWT User"
      ( '. nameField setText "GWT User" )

      ; getting obvious...
      ( 'local #{:final} Label (errorLabel ('new Label)) )
      ( '. sendButton addStyleName "sendButton" )

      ; of course expressions nest
      ; everything up to this point is using just percolator core
      ; here, 'add is a shorthand we defined above
      ; it is a percolator interpreter in the scope :statement
      ( 'add  ( '. RootPanel get "nameFieldContainer"  ) nameField )
      ( 'add  ( '. RootPanel get "sendButtonContainer" ) sendButton )
      ( 'add  ( '. RootPanel get "errorLabelContainer" ) errorLabel )

      ; some more trivial examples of user-defined interpreters
      ( 'focus nameField )
      ( 'select-all nameField )

      ; this user-defined interpreter is declaring a local variable of class
      ; DialogBox with the name dialogBox, and initializing it with the default
      ; constructor
      ( 'dialog-box dialogBox )

      ; calling methods on it
      ( '. dialogBox setText "RPC" )
      ( '. dialogBox setAnimationEnabled true )

      ; similarly declaring a local button
      ( 'button closeButton )

      ; set the DOM id of the element associated with the close button
      ( '. ( '. closeButton getElement ) setId "closeButton" )

      ( 'local #{:final} Label ( textToServerLabel ( 'new Label )) )
      ( 'local #{:final} HTML ( serverResponseLabel ( 'new HTML )) )

      ; make a vertical panel
      ( 'local #{} VerticalPanel ( dialogVPanel ( 'new VerticalPanel )) )

      ; give it a CSS class, and start adding things to it
      ( 'style dialogVPanel "dialogVPanel" )
      ; it'd be nice integrate hiccup here:
      ( 'add dialogVPanel ( 'new HTML "<b> Sending name to the server: </b>" ) )
      ( 'add dialogVPanel textToServerLabel )
      ; and removing the repetition with some new interpreters
      ( 'add dialogVPanel ( 'new HTML "<br><b> Server replies: </b>" ) )
      ( 'add dialogVPanel serverResponseLabel )
      ( '. dialogVPanel setHorizontalAlignment VerticalPanel/ALIGN_RIGHT )

      ; stuff dialog panel in dialog box
      ( 'set-widget dialogBox dialogVPanel )

      ; this does exactly what it looks like
      ( 'on-click closeButton
        ( 'hide dialogBox )
        ( 'enable sendButton )
        ( 'focus sendButton ))

      ( 'class #{} MyHandler
        ( 'implements ClickHandler KeyUpHandler )

        ( 'on-click
            ( '. this sendNameToServer )
            ( 'log "My balls are on fire" )
            )

        ; in this handler method, there's an if branch
        ; e.getNativeKeyCode() == KeyCodes/KEY_ENTER
        ( 'on-key-up
          ( 'if ( '==
                  ( '. e getNativeKeyCode )
                  KeyCodes/KEY_ENTER )
            (('. this sendNameToServer ))))

        ; okay, moment of truth, the user has pressed and released the <Enter> key
        ( 'method #{:public} void sendNameToServer []
          ( 'set-text errorLabel "" )

          ; accepting input from the user...
          ( 'local #{} String (textToServer ('. nameField getText)) )

          ; if it ain't right, complain and quit...
          ( 'if ('! ('. FieldVerifier isValidName textToServer))
              ( ('. errorLabel setText "I'm afraid I can't let you do that, Dave...")
                ( 'return )))

          ( 'set-text textToServerLabel textToServer )
          ( 'set-text serverResponseLabel "" )

          ; here we gonna call the GWT-generated client-side RPC jibber jabber
          ( 'async greeting/greet [(String result)]
            ; and in the unlikely event of the server actually responding...
            (( 'set-text dialogBox "We have received a message. It would appear that it is from the server, m'lud." )
             ( 'unstyle serverResponseLabel "serverResponseLabelError" )
             ( 'show-dialog result ))
            ; typical howling emptiness or bitter defeat...
            (( 'set-text dialogBox "something is wrong :(" )
             ( 'style serverResponseLabel "serverResponseLabelError" )
             ( 'show-dialog SERVER_ERROR )))
             )
        ); end class MyHandler

      ; instantiate one and bind it to events
      ( 'local #{} MyHandler ( handler ('new MyHandler )))
      ( '. sendButton addClickHandler handler )
      ( '. nameField addKeyUpHandler handler )

  )))
