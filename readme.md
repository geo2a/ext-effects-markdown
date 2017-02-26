Restricted Markdown parser and md-to-htmd translator. Implemented on top of [experimental parsers combinators library](https://github.com/geo2a/ext-effects-parsers) based on [Extensible Effects](https://github.com/suhailshergill/extensible-effects).

**Disclaimer:** This is not a production-ready code in any sense. Use it at you own risk (probably better don't)

## Features

* Headers
* Paragraphs
* Lists (non-nested)
* Block quotes
* Latex blocks

## Development

To build this project you need to have haskell [stack](http://docs.haskellstack.org/en/stable/README.html) installed.

One source [dependency](https://github.com/geo2a/ext-effects-parsers) is required. Please, clone the repo and put it in directory named `ext-effects-parsers` one level up, as required in `stack.yaml`, you can tweak this though.

To build just run:

```
stack build
```

To test:

```
stack test
```

To convert example markdown file to html:

```
stack exec ext-effects-markdown-exe -- examples/example.md -o examples/example.html
```