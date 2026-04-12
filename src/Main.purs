module Main where

import Prelude

import Affjax (Error)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Monad.ST.Class (liftST)
import Data.Array ((!!), mapWithIndex)
import Data.Array as Array
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Time.Duration (Seconds(..))
import Deku.Control (text, text_)
import Deku.Core (Nut, useRefST)
import Data.Tuple.Nested ((/\))
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (setTimeout)
import FRP.Poll (Poll)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, key)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- chars is pre-computed at build/load time so the keystroke handler
-- never calls toCharArray in the hot path.
type TypingExercise =
  { text        :: String
  , chars       :: Array Char
  , description :: String
  }

-- Delta-tracked so accuracy and progress are O(1) per keystroke.
type TypingStats =
  { correctCount   :: Int
  , incorrectCount :: Int
  , totalTyped     :: Int
  }

-- Single state atom: one Poll fire per keystroke instead of three.
-- Eliminates three separate downstream propagations through statsDisplay.
type KeystrokeState =
  { typed     :: String
  , wpm       :: Number
  , startTime :: Maybe Instant
  , stats     :: TypingStats
  }

initialKS :: KeystrokeState
initialKS =
  { typed:     ""
  , wpm:       0.0
  , startTime: Nothing
  , stats:     { correctCount: 0, incorrectCount: 0, totalTyped: 0 }
  }

--------------------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------------------

mkExercise :: String -> String -> TypingExercise
mkExercise desc t = { text: t, chars: CU.toCharArray t, description: desc }

defaultExercises :: Array TypingExercise
defaultExercises =
  [ mkExercise "Classic pangram"
      "The quick brown fox jumps over the lazy dog."
  , mkExercise "About Deku"
      "Deku is a push-based reactive UI framework for PureScript web applications."
  , mkExercise "About PureScript"
      "PureScript is a strongly-typed functional language that compiles to JavaScript."
  ]

emptyExercise :: TypingExercise
emptyExercise = { text: "", chars: [], description: "" }

--------------------------------------------------------------------------------
-- Text processing
-- Everything here runs once at load time. None of it touches the hot path.
--------------------------------------------------------------------------------

cleanText :: String -> String
cleanText input =
  let
    s0 = String.replaceAll (Pattern "\r\n")  (Replacement " ")   input
    s1 = String.replaceAll (Pattern "\n")     (Replacement " ")   s0
    s2 = String.replaceAll (Pattern "\x2018") (Replacement "'")   s1  -- left single quote
    s3 = String.replaceAll (Pattern "\x2019") (Replacement "'")   s2  -- right single quote
    s4 = String.replaceAll (Pattern "\x201C") (Replacement "\"")  s3  -- left double quote
    s5 = String.replaceAll (Pattern "\x201D") (Replacement "\"")  s4  -- right double quote
    s6 = String.replaceAll (Pattern "\x2013") (Replacement "-")   s5  -- en dash
    s7 = String.replaceAll (Pattern "\x2014") (Replacement "-")   s6  -- em dash
    s8 = String.replaceAll (Pattern "\x2026") (Replacement "...") s7  -- ellipsis
    -- Collapse whitespace runs via split/filter/join
    s9 = String.split (Pattern " ") s8
         # Array.filter (\w -> String.length (String.trim w) > 0)
         # String.joinWith " "
    -- Strip all non-printable and non-ASCII characters.
    -- CU.toCharArray/fromCharArray are safe here: cleanText guarantees ASCII output.
    isPrintable c = c >= ' ' && c <= '~'
  in
    String.trim $ CU.fromCharArray $ Array.filter isPrintable $ CU.toCharArray s9

-- Split text into word-boundary-aligned chunks of at most maxChars characters.
-- Pre-computes the chars field on each exercise to eliminate hot-path allocations.
chunkText :: Int -> String -> Array TypingExercise
chunkText maxChars content =
  let
    cleaned = cleanText content
    words   = Array.filter (\w -> String.length w > 0)
                $ String.split (Pattern " ") cleaned

    go :: Array String -> Int -> Array String -> Array TypingExercise -> Array TypingExercise
    go remaining curLen curWords acc =
      case Array.uncons remaining of
        Nothing ->
          if Array.null curWords then acc
          else acc <> [ finalize (Array.length acc + 1) curWords ]
        Just { head: w, tail: ws } ->
          let wLen   = String.length w
              -- +1 for the space separator that will be between words
              addLen = if Array.null curWords then wLen else curLen + 1 + wLen
          in
            if addLen > maxChars && not (Array.null curWords)
            then go ws wLen [ w ] (acc <> [ finalize (Array.length acc + 1) curWords ])
            else go ws addLen (curWords <> [ w ]) acc

    finalize :: Int -> Array String -> TypingExercise
    finalize n ws =
      let t = String.joinWith " " ws
      in { text: t, chars: CU.toCharArray t, description: "Chunk " <> show n }
  in
    case words of
      [] -> defaultExercises
      _  -> go words 0 [] []

--------------------------------------------------------------------------------
-- Delta stats: O(1) per keystroke
--
-- Rather than scanning the full typed string each keypress, we examine only
-- the single character that was added or removed and update counts accordingly.
--------------------------------------------------------------------------------

updateStats :: TypingStats -> String -> String -> Array Char -> TypingStats
updateStats prev oldTyped newTyped targetChars =
  let
    oldLen = CU.length oldTyped
    newLen = CU.length newTyped
  in
    if newLen > oldLen then
      -- Character appended: examine position (newLen - 1) only.
      let pos = newLen - 1
      in case (targetChars !! pos), (CU.charAt pos newTyped) of
        Just tc, Just ic ->
          prev { totalTyped    = newLen
               , correctCount   = prev.correctCount   + (if tc == ic then 1 else 0)
               , incorrectCount = prev.incorrectCount + (if tc /= ic then 1 else 0)
               }
        _, _ -> prev { totalTyped = newLen }

    else if newLen < oldLen then
      -- Character removed: un-count position (oldLen - 1) using oldTyped.
      let pos = oldLen - 1
      in case (targetChars !! pos), (CU.charAt pos oldTyped) of
        Just tc, Just ic ->
          prev { totalTyped    = newLen
               , correctCount   = prev.correctCount   - (if tc == ic then 1 else 0)
               , incorrectCount = prev.incorrectCount - (if tc /= ic then 1 else 0)
               }
        _, _ -> prev { totalTyped = newLen }

    else prev

--------------------------------------------------------------------------------
-- Rendering
--
-- THE KEY PERFORMANCE CONTRACT:
--
-- renderChunk is called once per chunk advance (infrequent, user-initiated).
-- It produces N spans whose DA.klass each subscribe to typedPoll.
--
-- On every keystroke, typedPoll fires and each span's className is written
-- via charClass. This is:
--   - N className string writes  (browser batches into one layout pass)
--   - Zero node allocations
--   - Zero node removals
--
-- charClass uses CU.charAt which is O(1) direct code-unit indexing.
-- cleanText guarantees all content is ASCII, so code-unit == code-point.
-- Using Data.String.CodePoints.codePointAt would be O(i) per call due to
-- UTF-16 walking; CU.charAt avoids that entirely.
--------------------------------------------------------------------------------

charClass :: Int -> Char -> String -> String
charClass i targetChar typed =
  let typedLen = CU.length typed
  in
    if i > typedLen     then "char char-not-typed"
    else if i == typedLen then "char char-cursor"
    else
      case CU.charAt i typed of
        Nothing -> "char char-not-typed"
        Just c  -> if c == targetChar then "char char-correct" else "char char-incorrect"

-- Spans are created once. Only DA.klass subscriptions fire on subsequent keystrokes.
renderChunk :: Array Char -> Poll String -> Array Nut
renderChunk targetChars typedPoll =
  mapWithIndex
    ( \i c ->
        D.span
          [ DA.klass $ charClass i c <$> typedPoll ]
          [ text_ (CU.singleton c) ]
    )
    targetChars

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

focusElementById :: String -> Effect Unit
focusElementById elemId = do
  doc <- window >>= document <#> toParentNode
  mEl <- querySelector (QuerySelector ("#" <> elemId)) doc
  case mEl of
    Just el -> case fromElement el of
      Just htmlEl -> focus htmlEl
      Nothing     -> pure unit
    Nothing -> pure unit

-- Round to one decimal place for display.
showRounded :: Number -> String
showRounded n = show (toNumber (floor (n * 10.0)) / 10.0)

loadTextFile :: String -> Aff (Either Error String)
loadTextFile path = map (map _.body) $ AX.get ResponseFormat.string path

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: Effect Unit
main = void $ runInBody Deku.do

  -- Primary mutable state nodes.
  setExercises    /\ exercises     <- useState defaultExercises
  setCurrentIndex /\ currentIndex  <- useState 0
  setKS           /\ keystrokeState <- useState initialKS
  setStatusMsg    /\ statusMsg     <- useState ""

  -- ST refs: read latest values inside Effect handlers without creating
  -- Poll subscriptions (which would cause the handler's own logic to
  -- trigger reactive re-renders).
  currentKSRef    <- useRefST initialKS       keystrokeState
  currentIndexRef <- useRefST 0               currentIndex
  exercisesRef    <- useRefST defaultExercises exercises

  -- Derived Polls, constructed once at startup.

  let currentExercise =
        (\exs i -> fromMaybe emptyExercise (exs !! i))
          <$> exercises
          <*> currentIndex

  -- typedPoll: the only Poll that renderChunk's DA.klass nodes subscribe to.
  -- Derived from keystrokeState so it fires exactly once per setKS call.
  let typedPoll = _.typed <$> keystrokeState

  -- statsDisplay fires once per keystroke (only keystrokeState changes while typing).
  -- currentExercise only changes on chunk advance, not while typing.
  let statsDisplay =
        ( \ex ks ->
            let total    = Array.length ex.chars
                accuracy = if ks.stats.totalTyped == 0 then 100.0
                           else   toNumber ks.stats.correctCount
                                / toNumber ks.stats.totalTyped
                                * 100.0
                progress = if total == 0 then 0.0
                           else min 100.0
                                $   toNumber ks.stats.totalTyped
                                  / toNumber total
                                  * 100.0
            in { progress, accuracy, wpm: ks.wpm }
        )
          <$> currentExercise
          <*> keystrokeState

  -- Advance to the next chunk, wrapping at the end.
  let advanceChunk :: Effect Unit
      advanceChunk = do
        idx <- liftST currentIndexRef
        exs <- liftST exercisesRef
        let newIdx = if idx >= (Array.length exs - 1) then 0 else idx + 1
        setKS initialKS
        setStatusMsg ""
        setCurrentIndex newIdx
        void $ setTimeout 50 $ focusElementById "typing-input"

  -- Reset the current chunk without advancing.
  let resetChunk :: Effect Unit
      resetChunk = do
        setKS initialKS
        setStatusMsg ""
        focusElementById "typing-input"

  -- Keystroke handler: the hot path.
  -- All reads are from ST refs (no subscriptions).
  -- All expensive work (stats, WPM) is O(1).
  -- A single setKS call emits one Poll propagation.
  let handleKeyPress :: KeyboardEvent -> Effect Unit
      handleKeyPress keyEvent = do
        ks  <- liftST currentKSRef
        idx <- liftST currentIndexRef
        exs <- liftST exercisesRef

        let ex          = fromMaybe emptyExercise (exs !! idx)
            targetChars = ex.chars            -- pre-computed Array Char, no allocation
            oldTyped    = ks.typed
            oldLen      = CU.length oldTyped
            targetLen   = Array.length targetChars
            keyPressed  = key keyEvent
            keyCode     = code keyEvent

        let newTyped
              -- Backspace: guard against negative take
              | keyCode == "Backspace"    = if oldLen > 0
                                            then CU.take (oldLen - 1) oldTyped
                                            else ""
              -- Block input once chunk is fully typed
              | oldLen >= targetLen       = oldTyped
              -- Space key returns a literal space character
              | keyCode == "Space"        = oldTyped <> " "
              -- Single printable character
              | CU.length keyPressed == 1 = oldTyped <> keyPressed
              | otherwise                 = oldTyped

        -- Start the WPM clock on first character; preserve it afterward.
        newStart <- case ks.startTime of
          Just t  -> pure (Just t)
          Nothing ->
            if CU.length newTyped > 0
            then Just <$> now
            else pure Nothing

        -- Live WPM. Suppressed for first 0.5s to avoid the initial spike
        -- when a single word has been typed and elapsed time is near zero.
        newWpm <- case newStart of
          Nothing -> pure 0.0
          Just startT -> do
            t <- now
            let (Seconds secs) = diff t startT :: Seconds
                -- Standard definition: 5 code units = 1 "word"
                wordsTyped      = toNumber (CU.length newTyped) / 5.0
            pure $ if secs > 0.5 then wordsTyped / (secs / 60.0) else 0.0

        -- O(1) delta stats: examine only the character that changed.
        let newStats = updateStats ks.stats oldTyped newTyped targetChars

        -- Single Poll fire. All three fields (typed, wpm, startTime) propagate
        -- together. statsDisplay and typedPoll each fire exactly once.
        setKS { typed: newTyped, wpm: newWpm, startTime: newStart, stats: newStats }

        -- Auto-advance on exact completion (string equality over ≤300 chars).
        when (targetLen > 0 && newTyped == ex.text) do
          setStatusMsg "\x2713 Complete!"
          void $ setTimeout 350 advanceChunk

  ---- DOM -----------------------------------------------------------------------
  --
  -- Static structure principle: every node that never needs to be replaced
  -- uses DA.klass_ or text_. Only text children of stat spans and DA.klass
  -- on character spans carry reactive subscriptions. <#~> appears exactly
  -- once, guarded by currentExercise (fires only on chunk advance).

  D.div [ DA.klass_ "typing-trainer-container" ]
    [
    -- Header
      D.div [ DA.klass_ "header" ]
        [ D.h1 [ DA.klass_ "app-title" ] [ text_ "Typing Trainer" ] ]

    -- Stats bar: three static spans, each with one reactive text child.
    -- Per-keystroke cost: three textNode.data writes via statsDisplay.
    , D.div [ DA.klass_ "stats-bar" ]
        [ D.span [ DA.klass_ "stat" ]
            [ text $ (\s -> "Progress: " <> showRounded s.progress <> "%") <$> statsDisplay ]
        , D.span [ DA.klass_ "stat-divider" ] [ text_ " | " ]
        , D.span [ DA.klass_ "stat" ]
            [ text $ (\s -> "Accuracy: " <> showRounded s.accuracy <> "%") <$> statsDisplay ]
        , D.span [ DA.klass_ "stat-divider" ] [ text_ " | " ]
        , D.span [ DA.klass_ "stat" ]
            [ text $ (\s -> "WPM: " <> showRounded s.wpm) <$> statsDisplay ]
        ]

    -- Chunk metadata: description, page counter, completion flash.
    -- These fire only on chunk advance or load, not during typing.
    , D.div [ DA.klass_ "exercise-header" ]
        [ D.span [ DA.klass_ "exercise-description" ]
            [ text $ _.description <$> currentExercise ]
        , D.span [ DA.klass_ "page-indicator" ]
            [ text $
                (\exs i -> "  [" <> show (i + 1) <> " / " <> show (Array.length exs) <> "]")
                  <$> exercises
                  <*> currentIndex
            ]
        , D.span [ DA.klass_ "status-msg" ] [ text statusMsg ]
        ]

    -- Text display.
    -- <#~> fires ONLY when the chunk changes (chunk advance or load).
    -- During typing, only DA.klass attributes fire via typedPoll.
    -- Per-keystroke cost: N className writes batched by the browser into
    -- a single layout pass. Zero node allocations or removals.
    , D.div [ DA.klass_ "text-display" ]
        [ currentExercise <#~> \ex ->
            D.div [ DA.klass_ "text-content" ]
              (renderChunk ex.chars typedPoll)
        ]

    -- Off-screen textarea: sole mechanism for capturing keystrokes.
    -- position:fixed removes it from document flow entirely.
    -- pointer-events:none prevents accidental focus loss on click.
    -- Spellcheck, autocomplete, and autocorrect suppressed to eliminate
    -- browser composition events and underline reflow passes.
    , D.textarea
        [ DA.klass_ "hidden-input"
        , DA.id_ "typing-input"
        , DA.style_
            ( "position:fixed;left:-9999px;width:1px;height:1px;"
            <> "opacity:0;pointer-events:none;"
            )
        , DA.autofocus_ "autofocus"
        , DL.keydown_ handleKeyPress
        ]
        []

    -- Controls
    , D.div [ DA.klass_ "controls" ]
        [ D.button
            [ DA.klass_ "btn btn-secondary"
            , DL.click_ $ const resetChunk
            ]
            [ text_ "Reset" ]

        , D.button
            [ DA.klass_ "btn btn-primary"
            , DL.click_ $ const advanceChunk
            ]
            [ text_ "Next Chunk" ]

        , D.button
            [ DA.klass_ "btn btn-load"
            , DL.click_ $ const $ launchAff_ do
                result <- loadTextFile "books/TheDispossessed_LeGuin.txt"
                liftEffect $ case result of
                  Left _ ->
                    setStatusMsg "Error: could not load book."
                  Right content -> do
                    let chunks = chunkText 300 content
                    if Array.length chunks > 0
                    then do
                      setExercises chunks
                      setCurrentIndex 0
                      setKS initialKS
                      setStatusMsg
                        $ "Loaded " <> show (Array.length chunks) <> " chunks."
                      void $ setTimeout 2000 (setStatusMsg "")
                    else
                      setStatusMsg "Error: no content found in file."
            ]
            [ text_ "Load Book" ]
        ]
    ]