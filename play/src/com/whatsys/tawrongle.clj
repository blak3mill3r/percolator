(ns com.whatsys.tawrongle
  ( :require [clojure.string :as string] )
  ( :use     [percolator.core]))

(compilation-unit com.whatsys.server

  [ com.whatsys.client.GreetingService
    com.whatsys.shared.FieldVerifier
    com.google.gwt.user.server.rpc.RemoteServiceServlet ]

  (class-decl #{:public} GreetingServiceImpl
    ( 'extends RemoteServiceServlet)
    ( 'implements GreetingService )
    ( 'method #{:public IllegalArgumentException} String greetServer [(String input)]
      ('if ('! ('. FieldVerifier isValidName input))
       (('throw ('new IllegalArgumentException "Not cool with the server, sorry")) )
       (('return "The server's nuts are on fire ooh!"))))))


(compilation-unit com.whatsys.shared []
  (class-decl #{:public} FieldVerifier
    ( 'method #{:public :static} boolean isValidName [(String name)]
      ('if ('== name null)
        (( 'return false )))
      ('return
        ( '> ('. name length) 5 )))))

(compilation-unit com.whatsys.client [com.google.gwt.user.client.rpc.AsyncCallback]
  (interface-decl #{:public} GreetingServiceAsync
    ( 'method #{ IllegalArgumentException } void greetServer [(String name) (AsyncCallback<String> callback)])))

(compilation-unit com.whatsys.client
  [ com.google.gwt.user.client.rpc.RemoteService
    com.google.gwt.user.client.rpc.RemoteServiceRelativePath ]
    (interface-decl #{:public [(RemoteServiceRelativePath "greet")]} GreetingService
      ( 'extends RemoteService )
      ( 'method #{ IllegalArgumentException } String greetServer [(String name)])))

