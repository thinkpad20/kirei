import Data.Monoid

joinBy sep list = case list of
  [] -> []
  [s] -> s
  s:ss -> s <> sep <> joinBy sep ss
