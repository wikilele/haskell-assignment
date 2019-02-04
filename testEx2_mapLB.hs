{-# LANGUAGE TemplateHaskell #-}
import Ex1
import Ex2

import Test.HUnit


const42 _ = 42

notwfbag = (LB [(5,1),(5,7),(1,3)])
wfbag = (LB [(5,1),(7,7),(2,8)])

testMapLB1 = TestCase $ assertBool "testMapLB1 failed" (wf (mapLB (*2) wfbag))
testMapLB2 = TestCase $ assertEqual "testMapLB2 failed" (wf (mapLB (*2) notwfbag)) False

testMapLB3 = TestCase $ assertEqual "testMapLB3 failed" (wf (mapLB (const42) wfbag)) False
testMapLB4 = TestCase $ assertEqual "testMapLB4 failed" (wf (mapLB (*0) wfbag)) False




testlist = TestList [ TestLabel "testMapLB1" testMapLB1,
                      TestLabel "testMapLB2" testMapLB2,
                      TestLabel "testMapLB3" testMapLB3,
                      TestLabel "testMapLB4" testMapLB4
                    ]


-- Main
main :: IO ()
main = do
  runTestTT testlist
  return ()