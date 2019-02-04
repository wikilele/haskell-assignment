{-# LANGUAGE TemplateHaskell #-}
import Ex1

import Test.HUnit

-- this tests check if the implemented functions return a well formed List Bag

notwfbag = (LB [(5,1),(5,7),(1,3)])
wfbag = (LB [(5,1),(7,7),(2,8)])


testWf1 = TestCase $ assertEqual "testWf1 failed" (wf notwfbag ) False
testWf2 = TestCase $ assertBool "testWf2 failed" (wf wfbag ) 

-- test empty skipped cause it's in testEx1.hs
-- test singleton skipped cause it's in testEx1.hs

-- add assumes that the fst elem of the tutple isn't in the listbag
testAdd1 = TestCase $ assertBool "testAdd1 failed" (wf (add (3,4) wfbag))
testAdd2 = TestCase $ assertEqual "testAdd2 failed" (wf (add (5,4) wfbag)) False

testAdd'1 = TestCase $ assertBool "testAdd'1 failed" (wf (add' (3,4) wfbag))
testAdd'2 = TestCase $ assertBool "testAdd'2 failed" (wf (add' (5,4) wfbag)) 

-- just testing the remove function
-- only in this case I'm not performing a 'well formed' test cause if the LB was well formed before the remove
-- it will surely be well formed after.
testRemove1 = TestCase $ assertEqual "testRemove1 failed" (remove 5 wfbag) (LB [(7,7),(2,8)])
testRemove2 = TestCase $ assertEqual "testRemove2 failed" (remove 100 wfbag) (wfbag)

testFromList = TestCase $ assertBool "testFromList failed" (wf (fromList "mississippi"))

testSumBag1 = TestCase $ assertEqual "testSumBag1 failed"  (wf (sumBag wfbag notwfbag)) False
testSumBag2 = TestCase $ assertBool "testSumBag2 failed"  (wf (sumBag wfbag wfbag)) 
 

testlist = TestList [ TestLabel "testWf1" testWf1,
                      TestLabel "testWf2" testWf2,
                      TestLabel "testAdd1" testAdd1,
                      TestLabel "testAdd2" testAdd2,
                      TestLabel "testAdd'1" testAdd'1,
                      TestLabel "testAdd'2" testAdd'2,
                      TestLabel "testRemove1" testRemove1,
                      TestLabel "testRemove1" testRemove2,
                      TestLabel "testFromList" testFromList,
                      TestLabel "testSumBag1" testSumBag1,
                      TestLabel "testSumBag2" testSumBag2
                    ]


-- Main
main :: IO ()
main = do
  runTestTT testlist
  return ()
