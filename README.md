# Analysis II Cheat Sheet

A Cheat Sheet for the Analysis II Course at ETHZ.

Written in [typst](https://typst.app/docs/).

## Download
You can download the latest version of the cheatsheet here:
- [Normal Version](https://gitlab.dominik-schwaiger.ch/api/v4/projects/9/jobs/artifacts/main/raw/cheat_sheet.pdf?job=build)
- [Cringe Version (without cats)](https://gitlab.dominik-schwaiger.ch/api/v4/projects/9/jobs/artifacts/main/raw/cheat_sheet-no_cats.pdf?job=build)

## Development

To compile the pdf run `typst watch cheat_sheet.typ --open`.

## Inputs

The document supports a couple of inputs to allow customization at build time. These include:

- `REV`: Revision name shown in the footer of the pdf, default is `local`. Mainly used in CI to show the git hash.
- `CATS`: Boolean ("true" or "false") which determine if the cat images should be included, default is false

### Example

To for example, specify the local git revision as the `REV`, you can do the following:

```bash
typst watch cheat_sheet.typ --open --input REV=$(git rev-parse --short HEAD)
```
