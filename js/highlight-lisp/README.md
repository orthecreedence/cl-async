highlight-lisp - Common Lisp syntax highlighter
===============================================
This is a syntax highlighter for Common Lisp written in Javascript. It is
completely themable via CSS (themes included).

The purpose of this is to make it really easy to embed beautiful Common Lisp 
code into a website with minimal effort.

[See the demo!](http://orthecreedence.github.com/highlight-lisp/)

Usage
-----
Usage is simple. You include `highlight-lisp.js`, link to one of the CSS themes,
and call one of highlight-lisp's highlighting functions:

```html
<!-- Put these in your document somewhere, probably in the head, although the <script>
     tag can probably go anywhere -->
<script type="text/javascript" src="/js/highlight-lisp/highlight-lisp.js"></script>
<link rel="stylesheet" href="/js/highlight-lisp/themes/github.css">

...

<!-- By default, the highlighter looks for code blocks with class="lisp" -->
<pre><code class="lisp">(defun test-syntax-highlighter ()
  "Docstring explaining what this function does."
  (let ((hash (make-hash-table :test #'equal)))
    ...))</pre></code>
```

Once the HTML is set up, there are a few ways to initialize highlighting:

```js
// automatically highlight all <code class="lisp">...</code> blocks
HighlightLisp.highlight_auto();

// specify a custom class name (instead of "lisp"):
HighlightLisp.highlight_auto({className: 'common-lisp'});

// highlight *every* code block
HighlightLisp.highlight_auto({className: null});

// manually highlight a code block
var code = document.getElementById('my-code-element');
HighlightLisp.highlight_element(code);
```

What gets highlighted
---------------------
- **Functions**  
CSS class `function`  
Anything starting with `(`: `(my-function ...)`
  - **Known functions**   
  CSS class `function known`  
  Any function known by the highlighter: things like `make-hash-table`, `when`,
  `format`, etc
  - **Special functions**  
  CSS class `function known special`  
  Mainly `let`, `let\*`, `lambda`.
  - **Symbol functions**  
  CSS class `function symbol`  
  Example: `#'my-function`
  - **Known symbol functions**  
  CSS class `function symbol known`  
  Examples: `#'equalp`, `#'format`
- **Keywords**  
CSS class `keyword`  
Anything starting with `:` like `:this-is-a-keyword `
  - **Known keywords**  
  CSS class `keyword known`  
  Known keywords are things like `:hash-keys`, `:supersede`, etc.
- **Symbols**  
CSS class `symbol`  
Anything starting with `'`: `'my-symbol`
- **Lambda-list operators**  
CSS class `lambda-list`  
Things like `&key`, `&body`, etc.
- **Numbers**  
CSS class `number`  
Any numbers: `69`, `-82.4`, `#xF047`, `#b11010`
  - **Integers**  
  CSS class `number integer`  
  Simple numbers: `42`, `867`, etc. (no decimals)
  - **Floats**  
  CSS class `number float`  
  Numbers with a decimal: `+47.82112`, `32.9` `3.` `.009`
  - **Hex**  
  CSS class `number hex`  
  Hex numbers: `#x8090`, `#xc001`
  - **Binary**  
  CSS class `number binary`  
  Example: `#b01101`
- **Variables**  
  By themselves, variables remain unhighlighted
  - **Known variables**  
  CSS class `variable known`  
  Examples: `*package*`, `*standard-output*`, etc
  - **Global variables**  
  CSS class `variable global`  
  Any symbol surrounded by `\*`: `*main-datastore*`, `*my-thread-local*`, etc
  - **Constants**  
  CSS class `variable constant`  
  Any symbol surrounded by `+`: `+dt+`, `+contant-time+`, etc
- **nil/t**  
CSS class `nil`  
Any standalone `nil` or `t` will get this class
- **Comments**  
CSS class `comment`  
Example: `; this is a comment`
- **Strings**  
CSS class `string`  
Anthing inside `"`: `"This is a string."`
- **Parens**  
CSS class `list`  
May be overkill, but any `(` or `)` characters are classified.

On that note, things that *don't get highlighted/aren't properly highlighted*:

- Variables...things like `let` bindings or other symols within code that would
be interpreted as variables. Highlighting these would most likely be prohibitive
in terms of time (not the mention the return on investment). Feel free to patch!
- Some number notations. For instance `0.44d0`.
- Multi-line comments `#| ... |#` are unsupported
- Many constants (such as `pi`, `internal-time-units-per-second`) are classified
as functions, not known variables. This is because I pulled the list out of my
vim highlight script, and couldn't find a list of "Common Lisp standard
variables" to cross reference with. I pulled out the ones I know of and put them
into the known variables list, but there are no doubt more. If you see something
that is a known variable but gets treated as a known function, please open a
github issue.

Why
---
> Aren't there a bunch of Javascript syntax highlighters out there already?

Yes, but truth be told, most ignore lisp. You can write custom parsers for some
of them, but the APIs they provide didn't work well enough for me. [highlight.js](http://softwaremaniacs.org/soft/highlight/en/)
has a very nice lisp highlighting mode, along with really nice themes, but I
wanted more control over the process.

For instance, `highlight-lisp` started as a [SyntaxHighlighter](http://alexgorbatchev.com/SyntaxHighlighter/)
brush, but I quickly realized that because of the limitations of Javascript not
allowing real [lookbehind regular expressions](http://www.regular-expressions.info/lookaround.html),
I needed more direct control over the search/replace process.

What I discovered was that given the proper tools, parsing lisp is *easy*
(in fact, a cake walk after just releasing [markdown.cl](https://github.com/orthecreedence/markdown.cl))
and there's no need for a big highlighting framework. You plug in some regexes,
slap some <span class="..."> tags around certain things, and call it a day.

License
-------
As always, MIT.


