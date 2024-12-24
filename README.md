# mrc.el -- tools for editing MARC21

This adds convenience functions for editing MARC21 data. Most of it is concerned with converting MARCBreaker data to XML or JSON, to save typing.

## Commands
- `mrc-insert-ruler`: Insert a ruler indicating character positions. For naked-eye-navigation in fixed fields.
- `mrc-mrk->json`: convert MARCBreaker to JSON-Object.
- `mrc-mrk->xml`: convert MARCBreaker to MARC-XML

## Keybindings
It's up to the user to bind keys to the commands. As I am using Doom Emacs, `mrc-doom-bind-keys` binds keys to the local leader maps of `nxml-mode`, `typescript-mode`, `rsjx-mode` and `js-json-mode`. If you are using Doom Emacs too, you can call this function in your config.

## Implementation (notes to future self)
Lines of MARCBreaker are read into an internal representation that can be serialized as JSON. 

So this string:

``` text
24500$$aMain Title $$b remainder of title $$c statement of responsibility
```

is read into

``` emacs-lisp
'((tag . "245")
  (ind1 . "0")
  (ind2 . "0")
  (subfields . [((code . "a") (value . "Main title"))
                ((code . "b") (value . "remainder of title"))
                ((code . "c") (value . "statement of responsibility"))]))
```

which can be serialized with `(json-serialize ...)`.

To output MARC-XML, this object is converted to a dom-object:

``` emacs-lisp
'(datafield ((tag . "245")
             (ind1 . "0")
             (ind2 . "0"))
  (subfield ((code . "a")) "Main Title")
  (subfield ((code . "b")) "remainder of title")
  (subfield ((code . "c")) "statement of responsibility"))
```

which can be printed as XML with `(dom-print DOM t t)`
