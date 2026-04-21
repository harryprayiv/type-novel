module Main
  ( CharResult(..)
  , charResult
  , charClassKS
  , chunkText
  , cleanText
  , updateStats
  , main
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Monad.ST.Class (liftST)
import Data.Array ((!!), mapWithIndex)
import Data.Array as Array
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut, useRefST)
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
import Effect.Ref as Ref
import Effect.Timer (TimeoutId, clearTimeout, setTimeout)
import FRP.Poll (Poll)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, isComposing, key)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Integer sentinel: no string construction in the classification hot path.
-- resultClass does one pattern-match to a string literal per subscription fire.
data CharResult = NotTyped | Cursor | Correct | Incorrect

derive instance Eq CharResult

instance Show CharResult where
  show NotTyped  = "NotTyped"
  show Cursor    = "Cursor"
  show Correct   = "Correct"
  show Incorrect = "Incorrect"

type TypingExercise =
  { text        :: String
  , chars       :: Array Char    -- pre-split at load time; no CU.toCharArray in hot path
  , charStrings :: Array String  -- pre-computed CU.singleton per char
  , description :: String
  }

type TypingStats =
  { correctCount   :: Int
  , incorrectCount :: Int
  , totalTyped     :: Int
  }

-- Single atom: one Poll fire per keystroke, one propagation through the graph.
-- Collapsing typed/wpm/startTime/stats into one record eliminates three
-- separate downstream fan-outs that the previous three-setter design caused.
type KeystrokeState =
  { typed         :: String
  , wpm           :: Number
  , startTime     :: Maybe Instant
  , stats         :: TypingStats
  , keyTimestamps :: Array Instant  -- newest-first ring buffer, capped at 60
  , isTyping      :: Boolean        -- true while typing; suppresses caret blink
  }

-- Produced on chunk completion; drives the results overlay.
type CompletionResult =
  { wpm         :: Number
  , accuracy    :: Number
  , timeSecs    :: Number
  , errors      :: Int
  , chunkNum    :: Int
  , totalChunks :: Int
  }

--------------------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------------------

emptyStats :: TypingStats
emptyStats = { correctCount: 0, incorrectCount: 0, totalTyped: 0 }

initialKS :: KeystrokeState
initialKS =
  { typed:         ""
  , wpm:           0.0
  , startTime:     Nothing
  , stats:         emptyStats
  , keyTimestamps: []
  , isTyping:      false
  }

mkExercise :: String -> String -> TypingExercise
mkExercise desc t =
  let cs = CU.toCharArray t
  in { text: t, chars: cs, charStrings: map CU.singleton cs, description: desc }

emptyExercise :: TypingExercise
emptyExercise = mkExercise "" ""

defaultExercises :: Array TypingExercise
defaultExercises =
  [ mkExercise "Classic pangram"
      "The quick brown fox jumps over the lazy dog."
  , mkExercise "About Deku"
      "Deku is a push-based reactive UI framework for PureScript web applications."
  , mkExercise "About PureScript"
      "PureScript is a strongly-typed functional language that compiles to JavaScript."
  ]

--------------------------------------------------------------------------------
-- Text processing
-- Runs once at load time. Never called during typing.
--------------------------------------------------------------------------------

cleanText :: String -> String
cleanText input =
  let
    s0 = String.replaceAll (Pattern "\r\n")   (Replacement " ")    input
    s1 = String.replaceAll (Pattern "\n")      (Replacement " ")    s0
    s2 = String.replaceAll (Pattern "\x2018")  (Replacement "'")    s1  -- left single quote
    s3 = String.replaceAll (Pattern "\x2019")  (Replacement "'")    s2  -- right single quote
    s4 = String.replaceAll (Pattern "\x201C")  (Replacement "\"")   s3  -- left double quote
    s5 = String.replaceAll (Pattern "\x201D")  (Replacement "\"")   s4  -- right double quote
    s6 = String.replaceAll (Pattern "\x2013")  (Replacement "-")    s5  -- en dash
    s7 = String.replaceAll (Pattern "\x2014")  (Replacement "-")    s6  -- em dash
    s8 = String.replaceAll (Pattern "\x2026")  (Replacement "...")  s7  -- ellipsis
    s9 = String.split (Pattern " ") s8
         # Array.filter (\w -> String.length (String.trim w) > 0)
         # String.joinWith " "
    isPrintable c = c >= ' ' && c <= '~'
  in
    String.trim $ CU.fromCharArray $ Array.filter isPrintable $ CU.toCharArray s9

-- Splits cleaned text into word-boundary-aligned chunks of at most maxChars
-- characters. Pre-computes chars and charStrings on each exercise so the
-- keystroke handler never allocates arrays.
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
              addLen = if Array.null curWords then wLen else curLen + 1 + wLen
          in
            if addLen > maxChars && not (Array.null curWords)
            then go ws wLen [ w ] (acc <> [ finalize (Array.length acc + 1) curWords ])
            else go ws addLen (curWords <> [ w ]) acc

    finalize :: Int -> Array String -> TypingExercise
    finalize n ws =
      let t  = String.joinWith " " ws
          cs = CU.toCharArray t
      in { text: t, chars: cs, charStrings: map CU.singleton cs
         , description: "Chunk " <> show n }
  in
    case words of
      [] -> defaultExercises
      _  -> go words 0 [] []

--------------------------------------------------------------------------------
-- Delta stats: O(1) per keystroke
-- Examines exactly one character position per event instead of scanning
-- the full typed string.
--------------------------------------------------------------------------------

updateStats :: TypingStats -> String -> String -> Array Char -> TypingStats
updateStats prev oldTyped newTyped targetChars =
  let
    oldLen = CU.length oldTyped
    newLen = CU.length newTyped
  in
    if newLen > oldLen then
      let pos = newLen - 1
      in if pos >= Array.length targetChars
         then prev { totalTyped = newLen }
         else case (targetChars !! pos), (CU.charAt pos newTyped) of
           Just tc, Just ic ->
             prev { totalTyped    = newLen
                  , correctCount   = prev.correctCount   + (if tc == ic then 1 else 0)
                  , incorrectCount = prev.incorrectCount + (if tc /= ic then 1 else 0)
                  }
           _, _ -> prev { totalTyped = newLen }

    else if newLen < oldLen then
      let pos = oldLen - 1
      in if pos >= Array.length targetChars
         then prev { totalTyped = newLen }
         else case (targetChars !! pos), (CU.charAt pos oldTyped) of
           Just tc, Just ic ->
             prev { totalTyped    = newLen
                  , correctCount   = prev.correctCount   - (if tc == ic then 1 else 0)
                  , incorrectCount = prev.incorrectCount - (if tc /= ic then 1 else 0)
                  }
           _, _ -> prev { totalTyped = newLen }

    else prev

--------------------------------------------------------------------------------
-- Rolling WPM
-- 10-second sliding window over a newest-first ring buffer of keystroke
-- timestamps. Returns 0 until enough data has accumulated to be meaningful.
--------------------------------------------------------------------------------

rollingWpm :: Array Instant -> Instant -> Number
rollingWpm timestamps currentTime =
  let
    windowSecs = 10.0
    recent = Array.filter
      ( \t -> let (Seconds age) = diff currentTime t :: Seconds
              in age <= windowSecs
      )
      timestamps
    count = Array.length recent
  in
    if count < 2 then 0.0
    else
      -- Buffer is newest-first; the oldest recent entry is the last element.
      case Array.last recent of
        Nothing    -> 0.0
        Just oldest ->
          let (Seconds elapsed) = diff currentTime oldest :: Seconds
          in if elapsed < 0.5 then 0.0
             -- Standard WPM definition: 5 code units = 1 word
             else (toNumber count / 5.0) / (elapsed / 60.0)

--------------------------------------------------------------------------------
-- Character classification and rendering
--
-- charResult: O(1) via CU.charAt (direct code-unit index).
-- cleanText guarantees ASCII output, so code-unit == code-point.
-- Using Data.String.CodePoints.codePointAt would be O(i) per call.
--
-- charClassKS: takes full KeystrokeState so isTyping can suppress caret blink.
-- Runs once per DA.klass subscription per setKS call.
--
-- renderChunk: builds N spans once per chunk advance.
-- Per-keystroke work is N className writes only; zero node allocation.
--------------------------------------------------------------------------------

charResult :: Int -> Char -> String -> CharResult
charResult i targetChar typed =
  let len = CU.length typed
  in
    if i > len       then NotTyped
    else if i == len then Cursor
    else case CU.charAt i typed of
      Nothing -> NotTyped
      Just c  -> if c == targetChar then Correct else Incorrect

displayChar :: Int -> Char -> KeystrokeState -> String
displayChar i targetChar ks =
  case charResult i targetChar ks.typed of
    Incorrect -> fromMaybe (CU.singleton targetChar)
                   (CU.singleton <$> CU.charAt i ks.typed)
    _         -> CU.singleton targetChar

charClassKS :: Int -> Char -> KeystrokeState -> String
charClassKS i c ks =
  case charResult i c ks.typed of
    NotTyped  -> "char char-not-typed"
    Correct   -> "char char-correct"
    Incorrect -> "char char-incorrect"
    Cursor    -> "char char-cursor" <> if ks.isTyping then " typing" else ""

renderChunk :: Array Char -> Poll KeystrokeState -> Array Nut
renderChunk targetChars ksPoll =
  mapWithIndex
    ( \i c ->
        D.span
          [ DA.klass $ charClassKS i c <$> ksPoll ]
          [ text $ displayChar i c <$> ksPoll ]
    )
    targetChars

--------------------------------------------------------------------------------
-- UI helpers
--------------------------------------------------------------------------------

focusInput :: Effect Unit
focusInput = do
  doc <- window >>= document <#> toParentNode
  mEl <- querySelector (QuerySelector "#typing-input") doc
  case mEl of
    Just el -> case fromElement el of
      Just htmlEl -> focus htmlEl
      Nothing     -> pure unit
    Nothing -> pure unit

showRounded :: Number -> String
showRounded n = show (toNumber (floor (n * 10.0)) / 10.0)

loadTextFile :: String -> Aff (Either String String)
loadTextFile filename = do
  result <- AX.get ResponseFormat.string ("/books/" <> filename)
  pure $ case result of
    Left err ->
      Left (AX.printError err)
    Right resp ->
      -- Vite with appType:"mpa" returns 404 for missing files, so Affjax
      -- gives us Left on a missing file. If you ever see this branch fire,
      -- it means the SPA fallback is still active somewhere.
      let body = resp.body
      in if String.take 15 body == "<!DOCTYPE html>"
            || String.take 5 body == "<html"
         then Left
                $ "Got HTML instead of text. "
               <> "Check that public/books/" <> filename
               <> " exists and vite.config.js has appType: \"mpa\"."
         else Right body

-- Static stat cell for the results card. Pure Nut, no reactive subscriptions.
resultStat :: String -> String -> Nut
resultStat label value =
  D.div [ DA.klass_ "result-stat" ]
    [ D.div [ DA.klass_ "result-stat-value" ] [ text_ value ]
    , D.div [ DA.klass_ "result-stat-label" ] [ text_ label ]
    ]

-- Chunk size toggle button. Reactive active state via sizePoll.
chunkSizeBtn :: Int -> (Int -> Effect Unit) -> Poll Int -> Nut
chunkSizeBtn size setter sizePoll =
  D.button
    [ DA.klass $ (\s -> "btn btn-sm" <> if s == size then " active" else "") <$> sizePoll
    , DL.click_ $ const $ do
        setter size
        focusInput
    ]
    [ text_ (show size) ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  -- Effect.Ref for the caret-idle timer. Needs to live outside Deku.do
  -- because TimeoutId has no Poll representation and requires imperative
  -- read/write across the cancel-and-reschedule pattern in the hot path.
  typingTimerRef <- Ref.new (Nothing :: Maybe TimeoutId)

  void $ runInBody Deku.do

    --------------------------------------------------------------------------
    -- State
    --------------------------------------------------------------------------

    setExercises    /\ exercises      <- useState defaultExercises
    setCurrentIndex /\ currentIndex   <- useState 0
    setKS           /\ keystrokeState <- useState initialKS
    setStatusMsg    /\ statusMsg      <- useState ""
    setCompletion   /\ completion     <- useState (Nothing :: Maybe CompletionResult)
    setChunkSize    /\ chunkSizeState <- useState 300
    setDarkTheme    /\ darkTheme      <- useState true

    --------------------------------------------------------------------------
    -- ST refs
    -- Read in Effect handlers without creating Poll subscriptions.
    -- Each useRefST creates an STRef that stays in sync with its Poll.
    --------------------------------------------------------------------------

    currentKSRef    <- useRefST initialKS                            keystrokeState
    currentIndexRef <- useRefST 0                                    currentIndex
    exercisesRef    <- useRefST defaultExercises                     exercises
    chunkSizeRef    <- useRefST 300                                  chunkSizeState
    darkThemeRef    <- useRefST true                                 darkTheme
    completionRef   <- useRefST (Nothing :: Maybe CompletionResult)  completion

    --------------------------------------------------------------------------
    -- Derived Polls
    --------------------------------------------------------------------------

    let currentExercise =
          (\exs i -> fromMaybe emptyExercise (exs !! i))
            <$> exercises <*> currentIndex

    -- Fires once per keystroke (keystrokeState) and once per chunk advance
    -- (currentExercise). The three stat text nodes are the only subscribers.
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
          ) <$> currentExercise <*> keystrokeState

    let containerClass =
          (\dark -> "typing-trainer-container" <> if dark then " dark" else " light")
            <$> darkTheme

    --------------------------------------------------------------------------
    -- Actions
    --------------------------------------------------------------------------

    let advanceChunk :: Effect Unit
        advanceChunk = do
          idx <- liftST currentIndexRef
          exs <- liftST exercisesRef
          let newIdx = if idx >= (Array.length exs - 1) then 0 else idx + 1
          setKS initialKS
          setStatusMsg ""
          setCompletion Nothing
          setCurrentIndex newIdx
          void $ setTimeout 50 focusInput

    let resetChunk :: Effect Unit
        resetChunk = do
          mTimer <- Ref.read typingTimerRef
          case mTimer of
            Just tid -> clearTimeout tid
            Nothing  -> pure unit
          Ref.write Nothing typingTimerRef
          setKS initialKS
          setStatusMsg ""
          setCompletion Nothing
          focusInput

    --------------------------------------------------------------------------
    -- Keystroke handler: the hot path
    --
    -- All state reads use ST refs (no subscriptions, no reactive side-effects).
    -- All expensive work (stats, WPM) is O(1).
    -- One setKS call emits one Poll propagation covering all downstream work.
    --
    -- When the results screen is visible, Enter/Space advance the chunk;
    -- all other keys are suppressed so the hidden textarea stays clean.
    --
    -- isComposing guard drops IME composition events (Japanese, Chinese, Korean,
    -- Mac dead-key sequences) before they reach the input logic.
    --------------------------------------------------------------------------

    let handleKeyPress :: KeyboardEvent -> Effect Unit
        handleKeyPress keyEvent =
          when (not (isComposing keyEvent)) do
            mComp <- liftST completionRef
            if isJust mComp
            then do
              -- Results screen is active: only Enter/Space advance.
              let kc = code keyEvent
              when (kc == "Enter" || kc == "Space") advanceChunk
            else do
              ks  <- liftST currentKSRef
              idx <- liftST currentIndexRef
              exs <- liftST exercisesRef

              let ex        = fromMaybe emptyExercise (exs !! idx)
                  targetLen = Array.length ex.chars
                  oldTyped  = ks.typed
                  oldLen    = CU.length oldTyped
                  kp        = key keyEvent
                  kc        = code keyEvent

              let newTyped
                    | kc == "Backspace"   = if oldLen > 0
                                            then CU.take (oldLen - 1) oldTyped
                                            else ""
                    | oldLen >= targetLen = oldTyped
                    | kc == "Space"       = oldTyped <> " "
                    | CU.length kp == 1  = oldTyped <> kp
                    | otherwise           = oldTyped

              -- Start clock on first character; preserve it afterward.
              newStart <- case ks.startTime of
                Just t  -> pure (Just t)
                Nothing ->
                  if CU.length newTyped > 0 then Just <$> now
                  else pure Nothing

              currentTime <- now

              -- Append to timestamp buffer only on character addition.
              let newTimestamps =
                    if CU.length newTyped > oldLen
                    then Array.take 60 $ Array.cons currentTime ks.keyTimestamps
                    else ks.keyTimestamps

              let newStats = updateStats ks.stats oldTyped newTyped ex.chars
              let newWpm   = rollingWpm newTimestamps currentTime

              -- Caret idle management:
              -- Cancel the pending false-setting timer (if any), reschedule it.
              -- isTyping stays true as long as keystrokes keep arriving within
              -- 500ms of each other. Goes false exactly once per idle period.
              mPrev <- Ref.read typingTimerRef
              case mPrev of
                Just tid -> clearTimeout tid
                Nothing  -> pure unit
              tid <- setTimeout 500 do
                ks2 <- liftST currentKSRef
                setKS (ks2 { isTyping = false })
                Ref.write Nothing typingTimerRef
              Ref.write (Just tid) typingTimerRef

              -- Single Poll fire: all 300 DA.klass subs + 3 stat text nodes.
              setKS
                { typed:         newTyped
                , wpm:           newWpm
                , startTime:     newStart
                , stats:         newStats
                , keyTimestamps: newTimestamps
                , isTyping:      true
                }

              -- Completion: record result, show overlay. Do NOT auto-advance;
              -- the results screen requires deliberate user action to dismiss.
              when (targetLen > 0 && newTyped == ex.text) do
                t <- now
                let elapsed = case newStart of
                      Nothing -> 0.0
                      Just st -> let (Seconds s) = diff t st :: Seconds in s
                    finalAcc =
                      if newStats.totalTyped == 0 then 100.0
                      else   toNumber newStats.correctCount
                           / toNumber newStats.totalTyped
                           * 100.0
                setCompletion $ Just
                  { wpm:         newWpm
                  , accuracy:    finalAcc
                  , timeSecs:    elapsed
                  , errors:      newStats.incorrectCount
                  , chunkNum:    idx + 1
                  , totalChunks: Array.length exs
                  }

    --------------------------------------------------------------------------
    -- DOM
    --
    -- Static structure principle: every node that never changes uses DA.klass_
    -- or text_. Reactive subscriptions exist only where values actually change.
    -- <#~> appears twice: once for chunk advance, once for overlay toggle.
    --------------------------------------------------------------------------

    D.div [ DA.klass containerClass ]

      [ -- Header: title + theme toggle
        D.div [ DA.klass_ "header" ]
          [ D.h1 [ DA.klass_ "app-title" ] [ text_ "Typing Trainer" ]
          , D.button
              [ DA.klass_ "btn-theme"
              , DL.click_ $ const do
                  d <- liftST darkThemeRef
                  setDarkTheme (not d)
                  focusInput
              ]
              [ text $ (\d -> if d then "Light Mode" else "Dark Mode") <$> darkTheme ]
          ]

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

      -- Exercise metadata: description + page counter + status flash.
      -- Fires only on chunk advance or load, never during typing.
      , D.div [ DA.klass_ "exercise-header" ]
          [ D.span [ DA.klass_ "exercise-description" ]
              [ text $ _.description <$> currentExercise ]
          , D.span [ DA.klass_ "page-indicator" ]
              [ text $
                  (\exs i -> " [" <> show (i + 1) <> "/" <> show (Array.length exs) <> "]")
                    <$> exercises <*> currentIndex
              ]
          , D.span [ DA.klass_ "status-msg" ] [ text statusMsg ]
          ]

      -- Text display.
      -- <#~> fires ONLY when chunk changes (advance or load).
      -- During typing: N className writes, zero node allocations.
      , D.div [ DA.klass_ "text-display" ]
          [ currentExercise <#~> \ex ->
              D.div [ DA.klass_ "text-content" ]
                (renderChunk ex.chars keystrokeState)
          ]

      -- Results overlay.
      -- position: fixed in CSS; appears over everything when completion is set.
      -- <#~> fires once on completion, once on dismissal.
      -- Enter/Space on hidden textarea also dismisses (handled in handleKeyPress).
      , completion <#~> case _ of
          Nothing -> D.div [ DA.klass_ "results-hidden" ] []
          Just r  ->
            D.div [ DA.klass_ "results-overlay" ]
              [ D.div [ DA.klass_ "results-card" ]
                  [ D.h2 [ DA.klass_ "results-title" ]
                      [ text_ "\x2713 Chunk Complete" ]
                  , D.div [ DA.klass_ "results-grid" ]
                      [ resultStat "WPM"      (showRounded r.wpm)
                      , resultStat "Accuracy" (showRounded r.accuracy <> "%")
                      , resultStat "Time"     (showRounded r.timeSecs <> "s")
                      , resultStat "Errors"   (show r.errors)
                      ]
                  , D.p [ DA.klass_ "results-progress" ]
                      [ text_ $ "Chunk " <> show r.chunkNum
                             <> " of "   <> show r.totalChunks
                      ]
                  , D.p [ DA.klass_ "results-hint" ]
                      [ text_ "Press Enter, Space, or click Continue" ]
                  , D.button
                      [ DA.klass_ "btn btn-primary results-continue"
                      , DL.click_ $ const advanceChunk
                      ]
                      [ text_ "Continue \x2192" ]
                  ]
              ]

      -- Off-screen textarea: sole keystroke capture mechanism.
      -- pointer-events: none prevents accidental focus steal on click.
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
              [ text_ "Next" ]

          , D.button
              [ DA.klass_ "btn btn-load"
              , DL.click_ $ const $ launchAff_ do
                  result <- loadTextFile "LeGuin_TheDispossessed.txt"
                  liftEffect $ case result of
                    Left err ->
                      setStatusMsg $ "Error: " <> err
                    Right content -> do
                      cs <- liftST chunkSizeRef
                      let chunks = chunkText cs content
                      if Array.length chunks > 0
                      then do
                        setExercises chunks
                        setCurrentIndex 0
                        setKS initialKS
                        setCompletion Nothing
                        setStatusMsg
                          $ "Loaded " <> show (Array.length chunks) <> " chunks."
                        void $ setTimeout 2000 (setStatusMsg "")
                        focusInput
                      else
                        setStatusMsg "Error: file was empty after cleaning."
              ]
              [ text_ "Load Book" ]

          -- Chunk size selector: applies on next book load.
          , D.div [ DA.klass_ "chunk-size-group" ]
              [ D.span [ DA.klass_ "chunk-size-label" ] [ text_ "Size:" ]
              , chunkSizeBtn 150 setChunkSize chunkSizeState
              , chunkSizeBtn 300 setChunkSize chunkSizeState
              , chunkSizeBtn 500 setChunkSize chunkSizeState
              ]
          ]
      ]