
class (Show a, Tupleable a) => Identifiable a where
  -- A unique identifier of a value stored in a database.
  data Key a :: Symbol
  --SchemaExpression
  data SchemaExpr = Define (AppRecordName a) a :$ UniqueConstraint '[Key a]
  --insert
  --get
  --update
  --delete

