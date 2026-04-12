module Test.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Effect (Effect)
import Main
  ( CharResult(..)
  , charResult
  , chunkText
  , cleanText
  , updateStats
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  cleanTextSpec
  chunkTextSpec
  updateStatsSpec
  charResultSpec

cleanTextSpec :: Spec Unit
cleanTextSpec = describe "cleanText" do

  it "leaves plain ASCII unchanged" do
    cleanText "Hello world." `shouldEqual` "Hello world."

  it "collapses multiple spaces" do
    cleanText "foo   bar" `shouldEqual` "foo bar"

  it "strips leading and trailing whitespace" do
    cleanText "  hello  " `shouldEqual` "hello"

  it "converts newline to space" do
    cleanText "foo\nbar" `shouldEqual` "foo bar"

  it "strips non-printable characters below space" do
    cleanText "hel\x01lo" `shouldEqual` "hello"

  it "preserves punctuation" do
    cleanText "Hello, world!" `shouldEqual` "Hello, world!"

  it "handles empty string" do
    cleanText "" `shouldEqual` ""

  it "handles whitespace-only string" do
    cleanText "   " `shouldEqual` ""

  it "strips DEL character" do
    cleanText "hel\x7flo" `shouldEqual` "hello"

  it "preserves tilde which is the last printable ASCII char" do
    cleanText "hello~world" `shouldEqual` "hello~world"

  it "preserves digits" do
    cleanText "abc123" `shouldEqual` "abc123"

  it "collapses mixed whitespace runs" do
    cleanText "a  b   c" `shouldEqual` "a b c"

  it "handles string with only non-printable characters" do
    cleanText "\x01\x02\x03" `shouldEqual` ""

chunkTextSpec :: Spec Unit
chunkTextSpec = describe "chunkText" do

  it "returns defaultExercises for empty content" do
    let chunks = chunkText 300 ""
    (Array.length chunks > 0) `shouldEqual` true

  it "returns defaultExercises for whitespace-only content" do
    let chunks = chunkText 300 "   "
    (Array.length chunks > 0) `shouldEqual` true

  it "single short word produces one chunk" do
    let chunks = chunkText 300 "hello"
    Array.length chunks `shouldEqual` 1

  it "no chunk exceeds maxChars" do
    let content = String.joinWith " " (Array.replicate 200 "word")
    let chunks = chunkText 300 content
    let allUnderLimit = Array.all (\ex -> String.length ex.text <= 300) chunks
    allUnderLimit `shouldEqual` true

  it "all words from content appear in chunks" do
    let content = "the quick brown fox jumps over the lazy dog"
    let chunks = chunkText 300 content
    let combined = String.joinWith " " (map _.text chunks)
    combined `shouldEqual` content

  it "chunk description is set correctly" do
    let chunks = chunkText 300 "hello world"
    case Array.head chunks of
      Just ex -> ex.description `shouldEqual` "Chunk 1"
      Nothing -> false `shouldEqual` true

  it "chars array length matches text length" do
    let chunks = chunkText 300 "hello"
    case Array.head chunks of
      Just ex -> Array.length ex.chars `shouldEqual` CU.length ex.text
      Nothing -> false `shouldEqual` true

  it "charStrings array length matches text length" do
    let chunks = chunkText 300 "hello"
    case Array.head chunks of
      Just ex -> Array.length ex.charStrings `shouldEqual` CU.length ex.text
      Nothing -> false `shouldEqual` true

  it "multiple chunks have incrementing descriptions" do
    let content = String.joinWith " " (Array.replicate 100 "word")
    let chunks = chunkText 20 content
    case Array.index chunks 1 of
      Just ex -> ex.description `shouldEqual` "Chunk 2"
      Nothing -> false `shouldEqual` true

  it "content that fits in one chunk produces exactly one chunk" do
    let chunks = chunkText 300 "short text"
    Array.length chunks `shouldEqual` 1

  it "single very long word stays in its own chunk" do
    let longword = String.joinWith "" (Array.replicate 50 "a")
    let chunks = chunkText 300 longword
    Array.length chunks `shouldEqual` 1

updateStatsSpec :: Spec Unit
updateStatsSpec = describe "updateStats" do

  let emptyStats = { correctCount: 0, incorrectCount: 0, totalTyped: 0 }
  let target = CU.toCharArray "hello"

  it "adding correct char increments correctCount" do
    let s = updateStats emptyStats "" "h" target
    s.correctCount `shouldEqual` 1
    s.incorrectCount `shouldEqual` 0
    s.totalTyped `shouldEqual` 1

  it "adding incorrect char increments incorrectCount" do
    let s = updateStats emptyStats "" "x" target
    s.correctCount `shouldEqual` 0
    s.incorrectCount `shouldEqual` 1
    s.totalTyped `shouldEqual` 1

  it "removing a correct char decrements correctCount" do
    let s1 = updateStats emptyStats "" "h" target
    let s2 = updateStats s1 "h" "" target
    s2.correctCount `shouldEqual` 0
    s2.incorrectCount `shouldEqual` 0
    s2.totalTyped `shouldEqual` 0

  it "removing an incorrect char decrements incorrectCount" do
    let s1 = updateStats emptyStats "" "x" target
    let s2 = updateStats s1 "x" "" target
    s2.incorrectCount `shouldEqual` 0
    s2.totalTyped `shouldEqual` 0

  it "same-length input leaves stats unchanged" do
    let s = updateStats emptyStats "h" "h" target
    s `shouldEqual` emptyStats

  it "typing past end of target only updates totalTyped" do
    let shortTarget = CU.toCharArray "hi"
    let s = updateStats { correctCount: 2, incorrectCount: 0, totalTyped: 2 } "hi" "hix" shortTarget
    s.totalTyped `shouldEqual` 3
    s.correctCount `shouldEqual` 2

  it "typing two correct chars accumulates correctly" do
    let s1 = updateStats emptyStats "" "h" target
    let s2 = updateStats s1 "h" "he" target
    s2.correctCount `shouldEqual` 2
    s2.totalTyped `shouldEqual` 2

  it "correctCount never goes negative" do
    let s = updateStats emptyStats "" "" target
    s.correctCount `shouldEqual` 0

  it "totalTyped equals correctCount plus incorrectCount after forward typing" do
    let s1 = updateStats emptyStats "" "h" target
    let s2 = updateStats s1 "h" "hx" target
    s2.totalTyped `shouldEqual` (s2.correctCount + s2.incorrectCount)

charResultSpec :: Spec Unit
charResultSpec = describe "charResult" do

  it "returns NotTyped for position beyond typed length" do
    charResult 5 'a' "" `shouldEqual` NotTyped

  it "returns Cursor at typed length boundary" do
    charResult 3 'a' "hel" `shouldEqual` Cursor

  it "returns Correct when chars match" do
    charResult 0 'h' "hello" `shouldEqual` Correct

  it "returns Incorrect when chars differ" do
    charResult 0 'h' "xello" `shouldEqual` Incorrect

  it "Cursor at position 0 for empty typed string" do
    charResult 0 'h' "" `shouldEqual` Cursor

  it "NotTyped for position 1 when typed is empty" do
    charResult 1 'e' "" `shouldEqual` NotTyped

  it "Correct for matching char in middle of word" do
    charResult 2 'l' "hel" `shouldEqual` Correct

  it "Incorrect for wrong char in middle of word" do
    charResult 2 'l' "hex" `shouldEqual` Incorrect

  it "NotTyped two positions past cursor" do
    charResult 5 'z' "hel" `shouldEqual` NotTyped