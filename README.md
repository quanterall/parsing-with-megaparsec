# parsing-with-megaparsec

This project is meant to showcase an example of a simple, stateless parser for a basic format
(`/etc/hosts`) in `src/HostsParser.hs` as well as a slighty more complex example of parsing a simple
scripting language (`src/ScriptingLanguage.hs`). The idea is for the scripting language example to
show that we might want to involve some state in our parsing (in this case for making sure that
identifiers that are accessed have been bound).

## Tests

There is a test suite for each of the files, accessible in `test/HostsParserSpec.hs` and
`test/ScriptingLanguageSpec.hs` respectively.
