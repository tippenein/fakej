import Protolude
import Lib
import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by QuickCheck)" []

unitTests = testGroup "Unit tests"
  [ testCase "runExpr on Add" $
      addExpr @?= SeqValue (SeqNum 2 (SeqNum 2 (SeqNum 2 End)))

  , testCase "runExpr on Fold" $
      foldExpr @?= NumValue 3

  , testCase "parse num" $
      (run "1") @?= (Num 1)

  , testCase "parse simple add" $
      run "1 1" @?= Seq (SeqNum 1 (SeqNum 1 End))

  , testCase "parse simple add" $
      run "1 1 + 1 1" @?= Add (SeqNum 1 (SeqNum 1 End)) (SeqNum 1 (SeqNum 1 End))

  , testCase "parse simple fold" $
      run "+/ 1 1" @?= Fold (SeqNum 1 (SeqNum 1 End))

  ]


addExpr :: Value
addExpr = eval $ Add s s
  where
    s = (SeqNum 1 (SeqNum 1 (SeqNum 1 End)))

foldExpr :: Value
foldExpr = eval $ Fold s
  where
    s = (SeqNum 1 (SeqNum 1 (SeqNum 1 End)))
