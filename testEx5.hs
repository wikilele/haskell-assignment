{-# LANGUAGE TemplateHaskell #-}
import Ex1
import Ex2
import Ex5

import Test.HUnit


simpleBag = LB [(1,2)]
wfbag = (LB [(5,1),(7,7),(2,8)])
constBag a = LB [(a - a,1)]

testReturnLB = TestCase $ assertEqual "testReturnLB failed" (returnLB 42) (LB [(42,1)])
testBindLB1 = TestCase $ assertEqual "testBindLB1 failed" (bindLB simpleBag myfun) (LB[(43,1)])
testBindLB2 = TestCase $ assertEqual "testBindLB2 failed" (bindLB wfbag constBag) (LB[(0,1),(0,1),(0,1)])
testBind = TestCase $ assertEqual "testBind failed" ((returnLB 42) >>= constBag) (LB[(0,1)])

testlist = TestList [ TestLabel "testReturnLB" testReturnLB,
                      TestLabel "testBindLB1" testBindLB1,
                      TestLabel "testBindLB2" testBindLB2,
                      TestLabel "testBind" testBind
                    ]


-- Main
main :: IO ()
main = do
  runTestTT testlist
  return ()