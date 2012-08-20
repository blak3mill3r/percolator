# percolator

## Warning
This works, but is still quite experimental. It may change radically. Comments are welcome.

## Q & A
**Q:** WTF?

**A:** Extensible Java metaprogramming

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

The reason it's so simple and small is that it's just a wrapper around Google's thoroughly spiffy library, [javaparser](http://code.google.com/p/javaparser/), and a tiny helpful smattering of Clojure.

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

(add-interpreters-to-scope :statement
  { 'on-click   on-click
    'hide       (interpreter [o]   `( '. ~o hide                  ))
    'enable     (interpreter [o]   `( '. ~o setEnabled true       ))
    'focus      (interpreter [o]   `( '. ~o setFocus true         ))
    })
```

## Status

It works. There's an example at [./play/src/com/whatsys/test.clj](https://github.com/blak3mill3r/percolator/blob/master/play/src/com/whatsys/test.clj) which compiles with the GWT compiler.

Here's the intermediate form [./play/src/com/whatsys/client/Play.java](https://github.com/blak3mill3r/percolator/blob/master/play/src/com/whatsys/client/Play.java)

## Notably missing

You cannot yet:

* define a class with generics
* You cannot express Java 'float' literals or literals of int types smaller than long
* probably a lot more...
* FIXME fill this in

## Usage

WYSIWYG. I've been experimenting with it in vim. It uses leiningen, and I use VimClojure. You might be able to toy with it the way I have been:
```bash
$ cd percolator/play
$ lein nailgun
$ vim src/com/whatsys/test.clj
```
and then eval percolator stuff in vim... if you are very fortunate, you might get compilable java source code in a vim buffer. Paste it into Play.java and try something along the lines of
```bash
$ ant devmode
```

...to start your GWT devmode server

If you try this out, I'd love to hear what you think.

## License

Copyright (C) 2012 Blake Miller

Distributed under the Eclipse Public License, the same as Clojure.
