module ProjectM36.Typed.Types where
import RIO
import qualified Data.UUID as UUID
import qualified System.Random as R
import Graph.Trace
-- @todo replace UUID with something else more cryptographically secure?

randomEtag :: g -> ((), g)
randomEtag g = ((), g)

instance R.Random () where
  randomR _ = R.random
  random = randomEtag

