package confuse

// Note: ideally we'd use only plain exports instead of a package object and
// inheritance. Because of a compiler bug however, exported mehods can lose
// their default parameters. Hence, until https://github.com/lampepfl/dotty/issues/17930
// we'll need to revert to using a package object.
object `package` extends api.MainApi:

  export confuse.model.Config
  export confuse.model.Value
  export confuse.model.Arr
  export confuse.model.Str
  export confuse.model.Path
  export confuse.model.Origin
  export confuse.model.Null

  object default extends parsers.DerivationApi with parsers.StandardParsers
