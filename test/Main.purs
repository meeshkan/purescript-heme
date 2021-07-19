module Test.Main where

import Prelude

import Data.Machine.Heme (heme)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

a :: String
a = "a"

b :: Unit -> Int
b _ = 42

c :: Unit ->Unit -> String
c _ _ = "c"

d :: Unit ->Unit ->Unit -> String
d _ _ _ = "d"

e :: Unit ->Unit ->Unit ->Unit -> String
e _ _ _ _ = "e"

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Tests heme" do
          it "works for simple example" do
            let
              chain = heme (d /\ c /\ a /\ b /\ unit)
              d' /\ cf' = chain unit unit unit
              c' /\ af' = cf' unit unit
              a' /\ bf' = af'
              b' /\ _ = bf' unit
            a' `shouldEqual` "a"
            b' `shouldEqual` 42
            c' `shouldEqual` "c"
            d' `shouldEqual` "d"
          it "works for record example" do
            let
              chain = heme (d /\ c /\ { aa: a, aaa: a } /\ b /\ unit)
              d' /\ cf' = chain unit unit unit
              c' /\ afs' = cf' unit unit
              aa' /\ bf' = afs'.aa
              aaa' /\ bf'' = afs'.aaa
              b' /\ _ = bf' unit
              b'' /\ _ = bf'' unit
            aa' `shouldEqual` "a"
            aaa' `shouldEqual` "a"
            b' `shouldEqual` 42
            b'' `shouldEqual` 42
            c' `shouldEqual` "c"
            d' `shouldEqual` "d"
