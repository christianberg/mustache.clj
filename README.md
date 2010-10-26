# mustache.clj

This is a Clojure implementation of the
[Mustache](http://mustache.github.com/) template language. 

It is not to be confused with the Clojure micro web framework
[Moustache](http://github.com/cgrand/moustache) by Christophe
Grand. (I'm sorry about the naming conflict, especially since Mustache
and Moustache are apparently equivalent to Google, but since this is
just an implementation of an existing templating language, I didn't
want to make up some new name.)

## Usage

The main function is `compile`. It takes a Mustache template in a
string as it's only argument and returns a function. This function
can be called with a "context" to render the data provided by the
context into the template. Usually the context will be a hash-map, but
it could be any function that returns the data to be rendered when
called with keyword arguments.

The function returned by `compile` renders directly to \*out\*, if you
need the output somewhere else, you must bind \*out\* accordingly.

    (def mytemplate (compile "Hello {{name}}!"))

    (mytemplate {:name "World"})  ;; prints "Hello World!" to *out*

There are two convience functions: `render` and
`render-to-string`. `render` takes a template string and a context as
arguments, internally it just compiles the templates and calls the
returned function right away:

    (render "Hello {{name}}!" {:name "World"}) ;; prints "Hello World!" to *out*

`render-to-string` does not print, but returns the rendered output (by
simply wrapping `render` in `with-out-str`:

    (render-to-string "Hello {{name}}!" {:name "World"}) ;; returns "Hello World!"

These functions are good for quickly rendering a template. If you need
to render a template many times with different data, it's probably
better to cache the result of `compile`, so the template parsing is
only done once.

## Features

The following Mustache features are currently implemented (also see
the TODO section):

* Templates can be provided as strings, URIs, Files, Streams, local
  file names or classpath resource names
* Variable tags (always HTML-escaped)
* Sections
** Render once for true values
** Don't render for false values
** Render once for maps, changing the context scope
** Render for each item in a collection
* Inverted sections
* Changing of tag delimiters
* Comments

## TODOs

The following features are not yet implemented:

* Unescaped output
* Partials
* Lambdas/Callbacks

## Installation

For now, get the source and play with it. I will push a package
to clojars as soon as the most important features are complete.

## Credits

* Mustache is Copyright (C) 2009 Chris Wanstrath
* HTML escaping code is copied from
  [hiccup](http://github.com/weavejester/hiccup) by James Reeves

## License

Copyright (C) 2010 Christian Berg

Distributed under the Eclipse Public License, the same as Clojure.
