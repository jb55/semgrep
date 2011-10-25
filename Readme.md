# semgrep

Semgrep is an experimental tool for building and querying generic ASTs.

The goal is to provide a tool for working with *programming languages*.

## Ideas

- All queries will be performed against the generic AST. Implementing a new
  langauge is just a matter of converting each AST node to an equivalent generic
  AST node. The generic AST must be very flexible to support multiple languages.

- Semantic grep. eg. Given a query `"calls to SomeFunction"` find all callsites
  for `SomeFunction`.

- Pandoc-style language conversion. Pretty printing a specific language
  from the generic AST is probably wishful thinking. This may be quite hard,
  need to look into this a bit more.
 
