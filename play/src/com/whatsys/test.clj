(ns com.whatsys.test
  ( :require [clojure.string :as string] )
  ( :use     [percolator.core]))


; in this case /body/ should be a string of javascript
(defn interpret-gwt-native-method [modifiers-and-annotations return-type method-name param-list body ]
  ( interpret-body-decl-method modifiers-and-annotations return-type method-name param-list body))

(defn interpret-gwt-panel [name forms]
  (binding [*perc-scope-args* { :gwt-panel name }]
    (doall ; do not forget this, force lazy-seq evaluations up front so they see the binding
      (map #( interpret-in-scope :gwt-panel % ) forms ))))

(inherit-scope       :gwt-panel       :statement     ) 
(inherit-scope       :statement       :gwt-statement ) 
(inherit-scope       :gwt-statement   :statement     ) 
(inherit-scope       :body-decl       :gwt-body-decl ) 
(inherit-scope       :gwt-body-decl   :body-decl     ) 

(inherit-scope-recur :gwt-statement                  ) 
(inherit-scope-recur :body-decl                      ) 

;scope-inheritance-wrappers

;(apply interpret-statement '(
;(new japa.parser.ast.stmt.ExpressionStmt (new japa.parser.ast.expr.MethodCallExpr (new japa.parser.ast.expr.NameExpr "Foohoho") "ass" []))
;  ))

(def interpret-halign
  { 'right 'VerticalPanel/ALIGN_RIGHT
    'left  'VerticalPanel/ALIGN_LEFT
   })

(definterpreter interpret-gwt-panel-keyword [k]
                (let [s (rest (.toString k) )
                      context (first s)
                      payload (apply str (rest s) )]
                  (if (= \. context )
                    `('. ~(:gwt-panel *perc-scope-args*) addStyleName ~payload)
                    (if (= \# context )
                      `( '. ( '. ~(:gwt-panel *perc-scope-args*) getElement ) setId ~payload )
                      (throw ( Throwable. (apply str "Misuse of clojure keyword " k " in gwt-panel, the keyword should start with a . or a #"  )))))))

(add-interpreters-to-scope :gwt-panel
  { 'style (interpreter [css-class] `('. ~(:gwt-panel *perc-scope-args*) addStyleName ~(interpret-expression css-class )) )
    'kazoo (interpreter [] `('. Foohoho ass))
    java.lang.String    (interpreter [s] `('. ~(:gwt-panel *perc-scope-args*) add ('new HTML ~s) ))
    clojure.lang.Symbol (interpreter [s] `('. ~(:gwt-panel *perc-scope-args*) add ~s ))
    clojure.lang.Keyword interpret-gwt-panel-keyword
    'h-align (interpreter [s] `( '. dialogVPanel setHorizontalAlignment ~(interpret-halign s) ))
   })

(add-interpreters-to-scope :statement
  { 'panelfy
      (interpreter [name & forms]
        `('block ~@(interpret-gwt-panel name forms)))})

; do this at some point....
;(def jt-async-callback-for-parameters [parameters]
;  )
     ;('new ~(jt-async-callback-for-parameters parameter-list) 
;AsyncCallback<String>

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
    'gwt-native-method  interpret-gwt-native-method
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

; japaparser outputs native methods as methods with a single expression
; statement in the body, which is a literal string expression of
; the string we want to become the native javascript body
; this regular expression replacement converts it to GWT JSNI syntax
; for obvious reasons this cannot be used in conjunction with other native
; methods, since it uses the word 'native' to match the method definitions
(defn hack-for-gwt-native-methods [java-code]
  (.replaceAll java-code "\n(.*native.*)\\{[.\\s]*\\\"(.*)\\\"\\;\\s*\\}" "$1/*-{$2}-*/;"))

(defmacro jsni-proxy [package-name class-name java-type-json-field-name-pairs & user-body-decls]
  (let [ jsni-wrapper-decl ; a function to make the method definition from the list of java-type/field-name pairs
           (interpreter [java-type-and-field-name]
             (let [ java-type         (first java-type-and-field-name)
                    field-name        (last java-type-and-field-name)
                    javascript-body   (apply str "return this." (.toString field-name ) ";" )
                    method-name       (apply str "js" (.replaceAll (.toString field-name ) "\\." "_" )) ]
               `( 'gwt-native-method #{:public :final :native} ~java-type ~method-name [] ~javascript-body )))
         extend-jso   [ '('extends JavaScriptObject)  ]
         nullary-ctor [ ( (interpreter [n] `('ctor #{:protected} ~n [] ) ) class-name )  ]
         jsni-wrapper-method-decls ( map jsni-wrapper-decl java-type-json-field-name-pairs )
        ]
    `(compilation-unit 
       { :postprocessors [ hack-for-gwt-native-methods ] }
       ~package-name
       [ com.google.gwt.core.client.JavaScriptObject ]
       (class-decl #{} ~class-name
         ~@extend-jso
         ~@nullary-ctor
         ~@jsni-wrapper-method-decls
         ~@user-body-decls ))))

(jsni-proxy
  com.whatsys.client StMessage
  [[ int id ] [ String body ] [ String created_at ] ]
  )

(jsni-proxy
  com.whatsys.client StStreamResponse
  [[ int id ] [ String body ] [ String created_at ] [ String user.username  ]]
  )

(compilation-unit
  ; metadata
  { :postprocessors [ hack-for-gwt-native-methods ] }
  ; package name
  com.whatsys.client
  ; imports
  [ com.whatsys.shared.FieldVerifier
    com.google.gwt.core.client.JsArray
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
    com.google.gwt.user.client.ui.VerticalPanel

    com.whatsys.client.StMessage

    com.google.gwt.http.client.Request
    com.google.gwt.http.client.RequestBuilder
    com.google.gwt.http.client.RequestCallback
    com.google.gwt.http.client.RequestException
    com.google.gwt.http.client.Response
   ]

  (class-decl #{:public} Play

   ( 'implements EntryPoint )

    ( 'field #{:private :static :final} String (SERVER_ERROR "D'oh!") )
    ( 'field #{:private :static :final} String (JSON_URL "http://127.0.0.1:8888/ststream.json") )

    ( 'field #{:private :final} GreetingServiceAsync ( greetingService ('gwt-new GreetingService) ) )

    ;( 'gwt-native-method #{:private :final :native} StStreamResponse streamResponse [( String json )]
    ;    "return eval(json);"
    ;    )

    ( 'gwt-native-method #{:private :final :native} JsArray<StMessage> getMessages [( String json )]
        "return eval(\"(\" +json + \")\").messages;"
        )

    ( 'method #{:public} void onModuleLoad []
      ( 'local #{:final} Button  (sendButton ('new Button "Send")))
      ( 'local #{:final} TextBox (nameField  ('new TextBox)))
      ( 'local #{:final} Label   (errorLabel ('new Label)))
      ( '. nameField setText "GWT User" )
      ( '. sendButton addStyleName "sendButton" )

      ( 'add  ( '. RootPanel get "nameFieldContainer"  ) nameField )
      ( 'add  ( '. RootPanel get "sendButtonContainer" ) sendButton )
      ( 'add  ( '. RootPanel get "errorLabelContainer" ) errorLabel )

      ( 'focus nameField )
      ( 'select-all nameField )

      ( 'dialog-box dialogBox )

      ( '. dialogBox setText "RPC" )
      ( '. dialogBox setAnimationEnabled true )

      ( 'button closeButton )

      ( '. ( '. closeButton getElement ) setId "closeButton" )

      ( 'local #{:final} Label ( textToServerLabel ( 'new Label )) )
      ( 'local #{:final} HTML ( serverResponseLabel ( 'new HTML )) )

      ( 'local #{} VerticalPanel ( dialogVPanel ( 'new VerticalPanel )) )

      ( 'panelfy dialogVPanel
        ;:#domidforpanel
        :.dialogVPanel
        "<b>My super sweet html blob</b>"
        textToServerLabel
        "<br><b> Server replies: </b>"
        serverResponseLabel
        ('h-align right))

      ( 'set-widget dialogBox dialogVPanel )

      ( 'on-click closeButton
        ( 'hide   dialogBox  ) 
        ( 'enable sendButton ) 
        ( 'focus  sendButton ))

      ( 'class #{} MyHandler
        ( 'implements ClickHandler KeyUpHandler )

        ( 'on-click
            ;( '. this sendNameToServer )
            ( '. this doSomeCrazyShit)
            ( 'log "My balls are on fire" )
            )

        ( 'on-key-up
          ( 'if ( '== ( '. e getNativeKeyCode ) KeyCodes/KEY_ENTER )
            (('. this sendNameToServer ))))

        ( 'method #{:public} void doSomeCrazyShit [] 
          ( 'local #{} RequestBuilder (builder ('new RequestBuilder RequestBuilder/GET JSON_URL)) )

          ( 'try (

          ( 'local #{} Request (request ('. builder sendRequest null
                                           ('new RequestCallback
                                              ('method #{:public} void onError [(Request request) (Throwable e)] 'empty )
                                              ('method #{:public} void onResponseReceived [(Request request) (Response response)]
                                                 ('if
                                                    ('== 200 ('. response getStatusCode ))
                                                    (
                                                     ( 'local #{} String ( responseText ('. response getText) ))
                                                     ( 'log responseText )
                                                     ( 'local #{} JsArray<StMessage> (calamity ('. nil getMessages responseText )))
                                                     ( 'log
                                                         ('.
                                                            ('. calamity get 0 )
                                                            jsbody)
                                                         )
                                                     
                                                     )
                                                    (('log "MEGAFAIL")))))))))
              ((RequestException e)
                 ('log "MEGAFAIL2")
                 )
          ))


        ; okay, moment of truth, the user has pressed and released the <Enter> key
        ( 'method #{:public} void sendNameToServer []
          ( 'set-text errorLabel "" )

          ; accepting input from the user...
          ( 'local #{} String (textToServer ('. nameField getText)) )

          ; if it ain't right, complain and quit...
          ( 'if ('! ('. FieldVerifier isValidName textToServer))
              ( ('. errorLabel setText "I'm afraid I can't let you do that, Bingus...")
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
             ( 'log ('. e toString ) )
             ( 'style serverResponseLabel "serverResponseLabelError" )
             ( 'show-dialog SERVER_ERROR )))
             )
        ); end class MyHandler

      ; instantiate one and bind it to events
      ( 'local #{} MyHandler ( handler ('new MyHandler )))
      ( '. sendButton addClickHandler handler )
      ( '. nameField addKeyUpHandler handler )

  )))


;(doseq [x (map #( .toString (eval ( interpret-body-decl % ) )) '(
;                              ;( '+ ( '- 5 7 ) 2 )
;                              ;( 'add somepanel ( '. somewhere getAWidget ) )
;  ( 'field #{:private :static :final} String (SERVER_ERROR "D'oo!") )
;  ( 'method #{:public} void onFailure [(Throwable e)] ( 'return 3))
;                              ))] ( println x ))
