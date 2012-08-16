# percolator

## Warning
This works, but is still quite experimental. It may change radically. Comments are welcome.

## Q & A
Q: WTF?
A: Extensible Java metaprogramming

This is a tool for writing arbitrarily complex compile-time logic for your Java projects. It is meant to become part of your Java build process, between you and the compiler.

## Synopsis
It's a library for working with Java AST structures in Clojure. It's for static
metaprogramming of Java code. It attempts to make that comprehensible, by
describing the code template as an embedded language in Clojure... certain
forms with special syntax represent Java AST nodes and their structure. This
can be mixed with arbitrary Clojure code. That Clojure code can, of course,
stick together Java AST however you like.

You can also *extend* percolator's syntax yourself, to make new shorthands for
repetive implementation details in your Java code ... bringing something like
**the power of Clojure macros** to help you **DRY up your Java code**.

If you've ever tried writing a program which vomits something into STDOUT which
you then > into a .java file and pass to javac or similar, and wished there
was a better way, this may be of interest.

## Under the hood
At the time of writing, this library is < 800 lines of .clj code. It covers a
good deal of the Java spec ... you can:

* **define** classes and interfaces: supports inheritance and mixins, inner classes, anonymous classes, class member variables and methods.
* **declare** variables of (most?) primitive types, or class types, supports every flag in Java, even those with peculiar names (an abstract, volatile, private, transient native? Is he protected?).
* **slag** together compilation units, attaching classes to them, and slagging them together into packages, importing things... I don't know if there's a proper verb for this, it's usually done by a programmer
* **metaprogram** unashamedly and without hesitation

The reason it's so simple and small is that it's just a wrapper around Google's thoroughly spiffy library, javaparser, and a bit of tasty Clojure sugar.
http://code.google.com/p/javaparser/

## A taste
This:
```java
    closeButton.addClickHandler(new ClickHandler() {
        public void onClick(ClickEvent e) {
            dialogBox.hide();
            sendButton.setEnabled(true);
            sendButton.setFocus(true);
        }
    });
```

can be expressed this concisely:
```clojure
    ( 'on-click closeButton
      ( 'hide   dialogBox  )
      ( 'enable sendButton )
      ( 'focus  sendButton ))
```

by extending percolator like:
```clojure
(definterpreter interpret-statement-on-click [identifier-symbol & statements]
 `( '. ~identifier-symbol addClickHandler
    ( 'new ClickHandler
      ( 'method #{:public} void onClick [ (ClickEvent e) ] ~@statements ))))

(add-statement-interpreters
  { '(quote on-click) (interpret-statement-on-click)
    '(quote hide)     (interpreter [o] `( '. ~o hide ))
    '(quote enable)   (interpreter [o] `( '. ~o setEnabled true ))
    '(quote focus)    (interpreter [o] `( '. ~o setFocus true )) })
```

## Status
It works. There's an example at ./play which compiles with the GWT compiler.

## Notably missing
You cannot yet:
* define a class with generics
FIXME fill this in

## Usage

WYSIWYG. I've been experimenting with it in vim. It uses leiningen. You might
be able to toy with it the way I have been:
$ cd percolator/play
$ lein vimclojure
$ vim src/com/whatsys/test.clj
and then <Leader>eb a visual block
Hopefully you get compilable java source code in a vim buffer.

If you try this out, I'd love to hear what you think.

## License

Copyright (C) 2012 Blake Miller

Distributed under the Eclipse Public License, the same as Clojure.
