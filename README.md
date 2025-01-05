# Analysis II Cheat Sheet

A Cheat Sheet for the Analysis II Course at ETHZ.

Written in [typst](https://typst.app/docs/).

## Development

To compile the pdf run `typst watch cheat_sheet.typ --open`.

## Inputs
The document supports a couple of inputs to allow customization at build time. These include:
- `REV`: Revision name shown in the footer of the pdf, default is `local`. Mainly used in CI to show the git hash.

### Example
To for example, specify the local git revision as the `REV`, you can do the following:
```
typst watch cheat_sheet.typ --open --input REV=$(git rev-parse --short HEAD)
```
