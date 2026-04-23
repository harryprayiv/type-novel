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
import Data.Argonaut.Core (jsonEmptyObject, stringify)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Array ((!!), mapWithIndex)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..), hush)
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
import Web.HTML.Window (document, innerHeight, innerWidth, localStorage)
import Web.Storage.Storage as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, isComposing, key)

--------------------------------------------------------------------------------
-- localStorage
--------------------------------------------------------------------------------

getItem :: String -> Effect (Maybe String)
getItem k = do
  w     <- window
  store <- localStorage w
  Storage.getItem k store

setItem :: String -> String -> Effect Unit
setItem k v = do
  w     <- window
  store <- localStorage w
  Storage.setItem k v store

removeItem :: String -> Effect Unit
removeItem k = do
  w     <- window
  store <- localStorage w
  Storage.removeItem k store

--------------------------------------------------------------------------------
-- Content hashing
-- djb2-style polynomial over 512 evenly-sampled characters.
-- O(1) regardless of file size.
--------------------------------------------------------------------------------

hashContent :: String -> Int
hashContent content =
  let
    len    = CU.length content
    stride = max 1 (len / 512)
    sample = Array.mapMaybe
               (\i -> CU.charAt (i * stride) content)
               (Array.range 0 511)
  in
    Array.foldl (\acc c -> acc * 31 + toCharCode c) 17 sample

hashKey :: Int -> String
hashKey h = "type-novel-book:" <> show h

--------------------------------------------------------------------------------
-- Per-book session progress
--------------------------------------------------------------------------------

lastActiveKey :: String
lastActiveKey = "type-novel-last-active"

type SessionRecord =
  { bookFile    :: String
  , contentHash :: Int
  , chunkIndex  :: Int
  , totalChunks :: Int
  }

encodeSession :: SessionRecord -> String
encodeSession sr =
  stringify $
       "bookFile"    := sr.bookFile
    ~> "contentHash" := sr.contentHash
    ~> "chunkIndex"  := sr.chunkIndex
    ~> "totalChunks" := sr.totalChunks
    ~> jsonEmptyObject

decodeSession :: String -> Maybe SessionRecord
decodeSession str = do
  json        <- hush (jsonParser str)
  obj         <- hush (decodeJson json)
  bookFile    <- hush (obj .: "bookFile")
  contentHash <- hush (obj .: "contentHash")
  chunkIndex  <- hush (obj .: "chunkIndex")
  totalChunks <- hush (obj .: "totalChunks")
  pure { bookFile, contentHash, chunkIndex, totalChunks }

getSessionFor :: Int -> Effect (Maybe SessionRecord)
getSessionFor h = do
  mStr <- getItem (hashKey h)
  pure $ mStr >>= decodeSession

saveSessionFor :: Int -> SessionRecord -> Effect Unit
saveSessionFor h sr = setItem (hashKey h) (encodeSession sr)

clearSessionFor :: Int -> Effect Unit
clearSessionFor = removeItem <<< hashKey

setLastActive :: SessionRecord -> Effect Unit
setLastActive sr = setItem lastActiveKey (encodeSession sr)

loadLastActive :: Effect (Maybe SessionRecord)
loadLastActive = do
  mStr <- getItem lastActiveKey
  pure $ mStr >>= decodeSession

--------------------------------------------------------------------------------
-- Lifetime stats
--------------------------------------------------------------------------------

lifetimeKey :: String
lifetimeKey = "type-novel-lifetime"

type LifetimeStats =
  { chunksCompleted :: Int
  , totalKeystrokes :: Int
  , totalErrors     :: Int
  , wpmSum          :: Number
  }

emptyLifetime :: LifetimeStats
emptyLifetime =
  { chunksCompleted: 0
  , totalKeystrokes: 0
  , totalErrors:     0
  , wpmSum:          0.0
  }

encodeLifetime :: LifetimeStats -> String
encodeLifetime ls =
  stringify $
       "chunksCompleted" := ls.chunksCompleted
    ~> "totalKeystrokes" := ls.totalKeystrokes
    ~> "totalErrors"     := ls.totalErrors
    ~> "wpmSum"          := ls.wpmSum
    ~> jsonEmptyObject

decodeLifetime :: String -> Maybe LifetimeStats
decodeLifetime str = do
  json            <- hush (jsonParser str)
  obj             <- hush (decodeJson json)
  chunksCompleted <- hush (obj .: "chunksCompleted")
  totalKeystrokes <- hush (obj .: "totalKeystrokes")
  totalErrors     <- hush (obj .: "totalErrors")
  wpmSum          <- hush (obj .: "wpmSum")
  pure { chunksCompleted, totalKeystrokes, totalErrors, wpmSum }

loadLifetime :: Effect LifetimeStats
loadLifetime = do
  mStr <- getItem lifetimeKey
  pure $ fromMaybe emptyLifetime (mStr >>= decodeLifetime)

saveLifetime :: LifetimeStats -> Effect Unit
saveLifetime = setItem lifetimeKey <<< encodeLifetime

lifetimeDisplay :: LifetimeStats -> { acc :: String, wpm :: String }
lifetimeDisplay ls =
  if ls.chunksCompleted == 0
  then { acc: "--", wpm: "--" }
  else
    { acc: showRounded
             $ toNumber (ls.totalKeystrokes - ls.totalErrors)
             / toNumber ls.totalKeystrokes
             * 100.0
    , wpm: showRounded (ls.wpmSum / toNumber ls.chunksCompleted)
    }

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data CharResult = NotTyped | Cursor | Correct | Incorrect

derive instance Eq CharResult

instance Show CharResult where
  show NotTyped  = "NotTyped"
  show Cursor    = "Cursor"
  show Correct   = "Correct"
  show Incorrect = "Incorrect"

type TypingExercise =
  { text        :: String
  , chars       :: Array Char
  , charStrings :: Array String
  , description :: String
  }

type TypingStats =
  { correctCount   :: Int
  , incorrectCount :: Int
  , totalTyped     :: Int
  , totalErrors    :: Int
  }

type KeystrokeState =
  { typed         :: String
  , wpm           :: Number
  , startTime     :: Maybe Instant
  , stats         :: TypingStats
  , keyTimestamps :: Array Instant
  , isTyping      :: Boolean
  }

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
emptyStats =
  { correctCount:   0
  , incorrectCount: 0
  , totalTyped:     0
  , totalErrors:    0
  }

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
--------------------------------------------------------------------------------

cleanText :: String -> String
cleanText input =
  let
    s0 = String.replaceAll (Pattern "\r\n")   (Replacement " ")    input
    s1 = String.replaceAll (Pattern "\n")      (Replacement " ")    s0
    s2 = String.replaceAll (Pattern "\x2018")  (Replacement "'")    s1
    s3 = String.replaceAll (Pattern "\x2019")  (Replacement "'")    s2
    s4 = String.replaceAll (Pattern "\x201C")  (Replacement "\"")   s3
    s5 = String.replaceAll (Pattern "\x201D")  (Replacement "\"")   s4
    s6 = String.replaceAll (Pattern "\x2013")  (Replacement "-")    s5
    s7 = String.replaceAll (Pattern "\x2014")  (Replacement "-")    s6
    s8 = String.replaceAll (Pattern "\x2026")  (Replacement "...")  s7
    s9 = String.split (Pattern " ") s8
         # Array.filter (\w -> String.length (String.trim w) > 0)
         # String.joinWith " "
    isPrintable c = c >= ' ' && c <= '~'
  in
    String.trim $ CU.fromCharArray $ Array.filter isPrintable $ CU.toCharArray s9

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

prettifyName :: String -> String
prettifyName filename =
  filename
    # String.replace    (Pattern ".txt") (Replacement "")
    # String.replaceAll (Pattern "_")    (Replacement " ")
    # String.replaceAll (Pattern "-")    (Replacement " ")

--------------------------------------------------------------------------------
-- Delta stats: O(1) per keystroke
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
             let isWrong = tc /= ic
             in prev { totalTyped    = newLen
                     , correctCount   = prev.correctCount   + (if isWrong then 0 else 1)
                     , incorrectCount = prev.incorrectCount + (if isWrong then 1 else 0)
                     , totalErrors    = prev.totalErrors    + (if isWrong then 1 else 0)
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
--------------------------------------------------------------------------------

rollingWpm :: Array Instant -> Instant -> Number
rollingWpm timestamps currentTime =
  let
    windowSecs = 10.0
    recent = Array.filter
      (\t -> let (Seconds age) = diff currentTime t :: Seconds
             in age <= windowSecs)
      timestamps
    count = Array.length recent
  in
    if count < 2 then 0.0
    else case Array.last recent of
      Nothing    -> 0.0
      Just oldest ->
        let (Seconds elapsed) = diff currentTime oldest :: Seconds
        in if elapsed < 0.5 then 0.0
           else (toNumber count / 5.0) / (elapsed / 60.0)

--------------------------------------------------------------------------------
-- Character classification and rendering
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

computeChunkSize :: Effect Int
computeChunkSize = do
  w  <- window
  vw <- innerWidth  w
  vh <- innerHeight w
  let availW       = toNumber vw * 0.92 - 84.0
      availH       = toNumber vh - 210.0
      charW        = 12.2
      lineH        = 37.0
      charsPerLine = floor (availW / charW)
      lines        = floor (availH / lineH)
  pure $ max 300 (charsPerLine * lines * 85 / 100)

fetchBookList :: Aff (Either String (Array String))
fetchBookList = do
  result <- AX.get ResponseFormat.json "/books/manifest.json"
  pure $ case result of
    Left err   -> Left (AX.printError err)
    Right resp -> lmap printJsonDecodeError (decodeJson resp.body)

loadTextFile :: String -> Aff (Either String String)
loadTextFile filename = do
  result <- AX.get ResponseFormat.string ("/books/" <> filename)
  pure $ case result of
    Left err -> Left (AX.printError err)
    Right resp ->
      let body = resp.body
      in if String.take 15 body == "<!DOCTYPE html>"
            || String.take 5 body == "<html"
         then Left
                $ "Got HTML instead of text. "
               <> "Check that public/books/" <> filename
               <> " exists and vite.config.js has appType: \"mpa\"."
         else Right body

resultStat :: String -> String -> Nut
resultStat label value =
  D.div [ DA.klass_ "result-stat" ]
    [ D.div [ DA.klass_ "result-stat-value" ] [ text_ value ]
    , D.div [ DA.klass_ "result-stat-label" ] [ text_ label ]
    ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  typingTimerRef   <- Ref.new (Nothing :: Maybe TimeoutId)
  initialChunkSize <- computeChunkSize
  initialLifetime  <- loadLifetime
  initialSession   <- loadLastActive
  activeHashRef    <- Ref.new (0 :: Int)

  void $ runInBody Deku.do

    --------------------------------------------------------------------------
    -- State
    --------------------------------------------------------------------------

    setExercises      /\ exercises      <- useState defaultExercises
    setCurrentIndex   /\ currentIndex   <- useState 0
    setKS             /\ keystrokeState <- useState initialKS
    setStatusMsg      /\ statusMsg      <- useState ""
    setCompletion     /\ completion     <- useState (Nothing :: Maybe CompletionResult)
    setDarkTheme      /\ darkTheme      <- useState true
    setAvailableBooks /\ availableBooks <- useState ([] :: Array String)
    setCurrentBook    /\ currentBook    <- useState (Nothing :: Maybe String)
    setLifetime       /\ lifetime       <- useState initialLifetime
    setSavedSession   /\ savedSession   <- useState initialSession
    setConfirmReset   /\ confirmReset   <- useState false

    --------------------------------------------------------------------------
    -- ST refs
    --------------------------------------------------------------------------

    currentKSRef    <- useRefST initialKS                           keystrokeState
    currentIndexRef <- useRefST 0                                   currentIndex
    exercisesRef    <- useRefST defaultExercises                    exercises
    darkThemeRef    <- useRefST true                                darkTheme
    completionRef   <- useRefST (Nothing :: Maybe CompletionResult) completion
    lifetimeRef     <- useRefST initialLifetime                     lifetime
    currentBookRef  <- useRefST (Nothing :: Maybe String)           currentBook

    --------------------------------------------------------------------------
    -- Derived Polls
    --------------------------------------------------------------------------

    let currentExercise =
          (\exs i -> fromMaybe emptyExercise (exs !! i))
            <$> exercises <*> currentIndex

    let statsDisplay =
          ( \ex ks ->
              let total    = Array.length ex.chars
                  accuracy = if ks.stats.totalTyped == 0 then 100.0
                             else   toNumber (ks.stats.totalTyped - ks.stats.totalErrors)
                                  / toNumber ks.stats.totalTyped
                                  * 100.0
                  progress = if total == 0 then 0.0
                             else min 100.0
                                  $   toNumber ks.stats.totalTyped
                                    / toNumber total
                                    * 100.0
              in { progress, accuracy, wpm: ks.wpm }
          ) <$> currentExercise <*> keystrokeState

    let lifetimeDisplay' = lifetimeDisplay <$> lifetime

    let containerClass =
          (\dark -> "typing-trainer-container" <> if dark then " dark" else " light")
            <$> darkTheme

    --------------------------------------------------------------------------
    -- Session persistence
    --------------------------------------------------------------------------

    let persistSession :: Int -> Array TypingExercise -> Effect Unit
        persistSession idx exs = do
          mBook <- liftST currentBookRef
          h     <- Ref.read activeHashRef
          case mBook of
            Nothing   -> pure unit
            Just file ->
              let sr = { bookFile:    file
                       , contentHash: h
                       , chunkIndex:  idx
                       , totalChunks: Array.length exs
                       }
              in do
                saveSessionFor h sr
                setLastActive sr
                setSavedSession (Just sr)

    --------------------------------------------------------------------------
    -- Actions
    --------------------------------------------------------------------------

    let goToChunk :: Int -> Effect Unit
        goToChunk newIdx = do
          exs <- liftST exercisesRef
          setKS initialKS
          setStatusMsg ""
          setCompletion Nothing
          setCurrentIndex newIdx
          persistSession newIdx exs
          void $ setTimeout 50 focusInput

    let advanceChunk :: Effect Unit
        advanceChunk = do
          idx <- liftST currentIndexRef
          exs <- liftST exercisesRef
          let newIdx = if idx >= (Array.length exs - 1) then idx else idx + 1
          goToChunk newIdx

    let prevChunk :: Effect Unit
        prevChunk = do
          idx <- liftST currentIndexRef
          let newIdx = if idx <= 0 then 0 else idx - 1
          goToChunk newIdx

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

    let resetBookProgress :: Effect Unit
        resetBookProgress = do
          h <- Ref.read activeHashRef
          clearSessionFor h
          removeItem lastActiveKey
          setSavedSession Nothing
          setConfirmReset false
          goToChunk 0

    let loadBook :: String -> Effect Unit
        loadBook filename = launchAff_ do
          liftEffect $ setStatusMsg $ "Loading " <> prettifyName filename <> "\x2026"
          result <- loadTextFile filename
          liftEffect $ case result of
            Left err -> setStatusMsg $ "Error: " <> err
            Right content -> do
              let h      = hashContent content
                  chunks = chunkText initialChunkSize content
              if Array.length chunks == 0
              then setStatusMsg "Error: file was empty after cleaning."
              else do
                mSession <- getSessionFor h
                let targetIdx = case mSession of
                      Just sr -> min sr.chunkIndex (Array.length chunks - 1)
                      Nothing -> 0
                    sr = { bookFile:    filename
                         , contentHash: h
                         , chunkIndex:  targetIdx
                         , totalChunks: Array.length chunks
                         }
                Ref.write h activeHashRef
                setExercises chunks
                setCurrentIndex targetIdx
                setKS initialKS
                setCompletion Nothing
                setConfirmReset false
                setCurrentBook (Just filename)
                setSavedSession (Just sr)
                saveSessionFor h sr
                setLastActive sr
                setStatusMsg $
                  prettifyName filename <>
                  ( if isJust mSession
                    then " - resuming chunk " <> show (targetIdx + 1)
                         <> "/" <> show (Array.length chunks) <> "."
                    else " - " <> show (Array.length chunks) <> " chunks."
                  )
                void $ setTimeout 3000 (setStatusMsg "")
                -- Delay past Deku's <#~> rebuilds from setExercises/setCurrentIndex
                -- before focusing, otherwise the DOM mutations steal focus back.
                void $ setTimeout 150 focusInput

    let resumeSession :: Effect Unit
        resumeSession = launchAff_ do
          mSr <- liftEffect loadLastActive
          case mSr of
            Nothing -> liftEffect $ setStatusMsg "No saved session found."
            Just sr -> liftEffect (loadBook sr.bookFile)

    let browseBooks :: Effect Unit
        browseBooks = launchAff_ do
          result <- fetchBookList
          liftEffect $ case result of
            Left err    -> setStatusMsg $ "Could not load book list: " <> err
            Right books -> do
              setAvailableBooks books
              if Array.null books
              then setStatusMsg "No .txt files found in public/books/"
              else setStatusMsg ""
          liftEffect focusInput

    --------------------------------------------------------------------------
    -- Keystroke handler
    --------------------------------------------------------------------------

    let handleKeyPress :: KeyboardEvent -> Effect Unit
        handleKeyPress keyEvent =
          when (not (isComposing keyEvent)) do
            when (code keyEvent == "Escape") do
              setCompletion Nothing
              setConfirmReset false

            mComp <- liftST completionRef
            if isJust mComp
            then do
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

              newStart <- case ks.startTime of
                Just t  -> pure (Just t)
                Nothing ->
                  if CU.length newTyped > 0 then Just <$> now
                  else pure Nothing

              currentTime <- now

              let newTimestamps =
                    if CU.length newTyped > oldLen
                    then Array.take 60 $ Array.cons currentTime ks.keyTimestamps
                    else ks.keyTimestamps

              let newStats = updateStats ks.stats oldTyped newTyped ex.chars
              let newWpm   = rollingWpm newTimestamps currentTime

              mPrev <- Ref.read typingTimerRef
              case mPrev of
                Just tid -> clearTimeout tid
                Nothing  -> pure unit
              tid <- setTimeout 500 do
                ks2 <- liftST currentKSRef
                setKS (ks2 { isTyping = false })
                Ref.write Nothing typingTimerRef
              Ref.write (Just tid) typingTimerRef

              setKS
                { typed:         newTyped
                , wpm:           newWpm
                , startTime:     newStart
                , stats:         newStats
                , keyTimestamps: newTimestamps
                , isTyping:      true
                }

              when (targetLen > 0 && newTyped == ex.text) do
                t <- now
                let elapsed = case newStart of
                      Nothing -> 0.0
                      Just st -> let (Seconds s) = diff t st :: Seconds in s
                    finalAcc =
                      if newStats.totalTyped == 0 then 100.0
                      else   toNumber (newStats.totalTyped - newStats.totalErrors)
                           / toNumber newStats.totalTyped
                           * 100.0

                ls <- liftST lifetimeRef
                let newLifetime =
                      { chunksCompleted: ls.chunksCompleted + 1
                      , totalKeystrokes: ls.totalKeystrokes + newStats.totalTyped
                      , totalErrors:     ls.totalErrors     + newStats.totalErrors
                      , wpmSum:          ls.wpmSum          + newWpm
                      }
                setLifetime newLifetime
                saveLifetime newLifetime

                setCompletion $ Just
                  { wpm:         newWpm
                  , accuracy:    finalAcc
                  , timeSecs:    elapsed
                  , errors:      newStats.totalErrors
                  , chunkNum:    idx + 1
                  , totalChunks: Array.length exs
                  }

    --------------------------------------------------------------------------
    -- DOM
    --------------------------------------------------------------------------

    D.div [ DA.klass containerClass ]

      [ D.div [ DA.klass_ "header" ]
          [ D.h1 [ DA.klass_ "app-title" ] [ text_ "type-novel" ]
          , D.button
              [ DA.klass_ "btn-theme"
              , DL.click_ $ const do
                  d <- liftST darkThemeRef
                  setDarkTheme (not d)
                  focusInput
              ]
              [ text $ (\d -> if d then "Light" else "Dark") <$> darkTheme ]
          ]

      , D.div [ DA.klass_ "stats-bar" ]
          [ D.div [ DA.klass_ "stats-row" ]
              [ D.span [ DA.klass_ "stat-group-label" ] [ text_ "Session" ]
              , D.span [ DA.klass_ "stat" ]
                  [ text $ (\s -> "Progress: " <> showRounded s.progress <> "%") <$> statsDisplay ]
              , D.span [ DA.klass_ "stat-divider" ] [ text_ "|" ]
              , D.span [ DA.klass_ "stat" ]
                  [ text $ (\s -> "Acc: " <> showRounded s.accuracy <> "%") <$> statsDisplay ]
              , D.span [ DA.klass_ "stat-divider" ] [ text_ "|" ]
              , D.span [ DA.klass_ "stat" ]
                  [ text $ (\s -> "WPM: " <> showRounded s.wpm) <$> statsDisplay ]
              , D.span [ DA.klass_ "stat-divider" ] [ text_ "|" ]
              , D.span [ DA.klass_ "stat page-inline" ]
                  [ text $
                      (\exs i -> show (i + 1) <> "/" <> show (Array.length exs))
                        <$> exercises <*> currentIndex
                  ]
              ]
          , D.div [ DA.klass_ "stats-row" ]
              [ D.span [ DA.klass_ "stat-group-label" ] [ text_ "Lifetime" ]
              , D.span [ DA.klass_ "stat lifetime-stat" ]
                  [ text $ (\d -> "Acc: " <> d.acc <> "%") <$> lifetimeDisplay' ]
              , D.span [ DA.klass_ "stat-divider" ] [ text_ "|" ]
              , D.span [ DA.klass_ "stat lifetime-stat" ]
                  [ text $ (\d -> "WPM: " <> d.wpm) <$> lifetimeDisplay' ]
              , D.span [ DA.klass_ "stat-divider" ] [ text_ "|" ]
              , D.span [ DA.klass_ "stat lifetime-stat" ]
                  [ text $ (\ls -> show ls.chunksCompleted <> " chunks") <$> lifetime ]
              ]
          ]

      , D.div [ DA.klass_ "exercise-header" ]
          [ D.span [ DA.klass_ "exercise-description" ]
              [ text $ _.description <$> currentExercise ]
          , D.span [ DA.klass_ "status-msg" ] [ text statusMsg ]
          ]

      , D.div [ DA.klass_ "text-display" ]
          [ currentExercise <#~> \ex ->
              D.div [ DA.klass_ "text-content" ]
                (renderChunk ex.chars keystrokeState)
          ]

      -- Completion overlay
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

      -- Reset confirmation overlay
      , confirmReset <#~> \asking ->
          if not asking
          then D.div [ DA.klass_ "results-hidden" ] []
          else
            D.div [ DA.klass_ "results-overlay" ]
              [ D.div [ DA.klass_ "results-card confirm-card" ]
                  [ D.h2 [ DA.klass_ "results-title confirm-title" ]
                      [ text_ "\x26A0 Reset Book Progress?" ]
                  , D.p [ DA.klass_ "confirm-body" ]
                      [ text $
                          (\cb -> case cb of
                            Nothing -> "No book is currently loaded."
                            Just f  -> "This will erase all saved progress for \""
                                    <> prettifyName f
                                    <> "\" and return to chunk 1. This cannot be undone."
                          ) <$> currentBook
                      ]
                  , D.div [ DA.klass_ "confirm-buttons" ]
                      [ D.button
                          [ DA.klass_ "btn btn-confirm-cancel"
                          , DL.click_ $ const do
                              setConfirmReset false
                              focusInput
                          ]
                          [ text_ "Cancel" ]
                      , D.button
                          [ DA.klass_ "btn btn-confirm-ok"
                          , DL.click_ $ const resetBookProgress
                          ]
                          [ text_ "Yes, Reset" ]
                      ]
                  , D.p [ DA.klass_ "confirm-hint" ]
                      [ text_ "Press Escape to cancel" ]
                  ]
              ]

      -- Off-screen textarea: sole keystroke capture mechanism.
      -- autofocus_ is set but cannot be relied upon for JS-rendered elements;
      -- explicit focusInput calls after all state updates are the real mechanism.
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

      , D.div [ DA.klass_ "controls" ]
          [ D.button
              [ DA.klass_ "btn btn-secondary"
              , DL.click_ $ const resetChunk
              ]
              [ text_ "Reset" ]

          , D.button
              [ DA.klass_ "btn btn-nav"
              , DL.click_ $ const prevChunk
              ]
              [ text_ "\x2190 Prev" ]

          , D.button
              [ DA.klass_ "btn btn-primary"
              , DL.click_ $ const advanceChunk
              ]
              [ text_ "Next \x2192" ]

          , D.button
              [ DA.klass_ "btn btn-load"
              , DL.click_ $ const browseBooks
              ]
              [ text_ "Browse Books" ]

          , D.button
              [ DA.klass_ "btn btn-danger"
              , DL.click_ $ const (setConfirmReset true)
              ]
              [ text_ "Reset Book" ]

          , savedSession <#~> case _ of
              Nothing -> D.div [ DA.klass_ "resume-empty" ] []
              Just sr ->
                D.button
                  [ DA.klass_ "btn btn-resume"
                  , DL.click_ $ const resumeSession
                  ]
                  [ text_ $
                      "Resume: " <> prettifyName sr.bookFile
                      <> " (" <> show (sr.chunkIndex + 1)
                      <> "/" <> show sr.totalChunks <> ")"
                  ]
          ]

      , availableBooks <#~> \books ->
          if Array.null books
          then D.div [ DA.klass_ "book-list-empty" ] []
          else D.div [ DA.klass_ "book-list" ]
                 ( books <#> \filename ->
                     D.button
                       [ DA.klass $
                           (\cb -> "btn btn-book" <>
                             if cb == Just filename then " active" else "")
                           <$> currentBook
                       , DL.click_ $ const (loadBook filename)
                       ]
                       [ text_ (prettifyName filename) ]
                 )
      ]

  -- autofocus on a JS-inserted element does not fire in any browser.
  -- Explicitly focus after the DOM has settled.
  void $ setTimeout 100 focusInput