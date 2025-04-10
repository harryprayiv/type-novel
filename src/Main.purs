module Main where

import Prelude

import Affjax (Error)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Monad.ST.Class (liftST)
import Data.Array (length, (!!), mapWithIndex, slice)
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, Pattern(..), Replacement(..), singleton, split)
import Data.String as String
import Data.String.CodePoints (toCodePointArray)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
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
import Effect.Timer (setTimeout)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, key)

data CharState = NotTyped | Correct | Incorrect

type TypingExercise = {
  text :: String,
  description :: String
}

defaultExercises :: Array TypingExercise
defaultExercises = [
  { 
    text: "The quick brown fox jumps over the lazy dog.",
    description: "Classic pangram"
  },
  { 
    text: "Deku is a UI framework that leverages a push-based reactive model for web applications.",
    description: "About Deku"
  },
  { 
    text: "PureScript is a strongly-typed functional programming language that compiles to JavaScript.",
    description: "About PureScript"
  }
]

loadTextFile :: String -> Aff (Either Error String)
loadTextFile path = do
  result <- AX.get ResponseFormat.string path
  pure $ map _.body result

splitIntoPages :: String -> Array TypingExercise
splitIntoPages content = 
  let
    cleanedContent = String.replace (Pattern "\r") (Replacement "") content
    
    paragraphs = split (Pattern "\n\n") cleanedContent
    
    createPage :: Array String -> Int -> Int -> Array TypingExercise -> Array TypingExercise
    createPage paras startIdx endIdx acc =
      if startIdx >= length paras then
        acc
      else
        let
          pageParas = slice startIdx (min endIdx (length paras)) paras
          
          pageText = String.joinWith "\n\n" pageParas
          
          nextStartIdx = if endIdx >= length paras
                         then endIdx
                         else endIdx
          
          nextEndIdx = if endIdx < length paras
                       then endIdx + 1
                       else endIdx
          
          cleanedText = cleanText pageText
          
          exercise = { 
            text: cleanedText, 
            description: "Page " <> show (length acc + 1)
          }
        in
          if String.length cleanedText > 0
          then createPage paras nextStartIdx nextEndIdx (acc <> [exercise])
          else createPage paras nextStartIdx nextEndIdx acc
  in
    createPage paragraphs 0 1 []

cleanText :: String -> String
cleanText text =
  let
    -- First, replace line breaks with spaces, ensuring word separation
    step1 = String.replaceAll (Pattern "\r\n") (Replacement " ") text
    step2 = String.replaceAll (Pattern "\n") (Replacement " ") step1
    
    -- Unicode character normalization
    step3 = String.replaceAll (Pattern "\x2018") (Replacement "'") step2   -- Left single quote
    step4 = String.replaceAll (Pattern "\x2019") (Replacement "'") step3   -- Right single quote
    step5 = String.replaceAll (Pattern "\x201C") (Replacement "\"") step4  -- Left double quote
    step6 = String.replaceAll (Pattern "\x201D") (Replacement "\"") step5  -- Right double quote
    step7 = String.replaceAll (Pattern "\x2013") (Replacement "-") step6   -- En dash
    step8 = String.replaceAll (Pattern "\x2014") (Replacement "-") step7   -- Em dash
    step9 = String.replaceAll (Pattern "\x2026") (Replacement "...") step8 -- Ellipsis
    
    -- Remove multiple consecutive spaces and ensure word separation
    step10 = String.replaceAll (Pattern "  ") (Replacement " ") step9
    step11 = String.replaceAll (Pattern "   ") (Replacement " ") step10
    
    -- Additional step to ensure words don't run together
    step12 = String.split (Pattern " ") step11
             # Array.filter (\s -> String.length (String.trim s) > 0)
             # String.joinWith " "
    
    -- Trim leading and trailing whitespace
    step13 = String.trim step12
    
    -- Only allow printable ASCII characters and basic whitespace
    isPrintable c = 
      (c >= ' ' && c <= '~') || c == ' '
  in
    fromCharArray $ Array.filter isPrintable $ toCharArray step13
    
-- Helper function to focus an element by ID
focusElementById :: String -> Effect Unit
focusElementById id = do
  doc <- window >>= document <#> toParentNode
  element <- querySelector (QuerySelector ("#" <> id)) doc
  case element of
    Just el -> case fromElement el of
      Just htmlEl -> focus htmlEl
      Nothing -> pure unit
    Nothing -> pure unit

main :: Effect Unit
main = void $ runInBody Deku.do
    -- State for exercises
    setExercises /\ exercises <- useState defaultExercises
    setCurrentExerciseIndex /\ currentExerciseIndex <- useState 0
    setTypedInput /\ typedInput <- useState ""
    
    -- refs to track current values
    currentInputRef <- useRefST "" typedInput
    currentIndexRef <- useRefST 0 currentExerciseIndex
    exercisesRef <- useRefST defaultExercises exercises
    
    -- Create a Poll for the current exercise
    let currentExercise = (Tuple <$> exercises <*> currentExerciseIndex) <#> \(Tuple exs i) -> 
          fromMaybe { text: "", description: "" } (exs !! i)
    
    let nextExercise = do
          liftEffect $ setTypedInput ""
          currentIdx <- liftST currentIndexRef
          exs <- liftST exercisesRef
          let newIndex = if currentIdx >= (length exs - 1) then 0 else currentIdx + 1
          liftEffect $ setCurrentExerciseIndex newIndex
          void $ liftEffect $ setTimeout 50 $ focusElementById "typing-input"
    
    let 
      handleKeyPress :: KeyboardEvent -> Effect Unit
      handleKeyPress keyEvent = do
        current <- liftST currentInputRef
        let keyPressed = key keyEvent
        let keyCode = code keyEvent
        let newInput = 
              if keyCode == "Backspace" then
                String.take (String.length current - 1) current
              else if String.length keyPressed == 1 || keyCode == "Space" then
                current <> if keyCode == "Space" then " " else keyPressed
              else
                current
        liftEffect $ setTypedInput newInput

    D.div 
      [ DA.klass_ "typing-trainer-container" ]
      [  
        -- Stats
        D.div [ DA.klass_ "stats" ]
          [ (Tuple <$> currentExercise <*> typedInput) <#~> \(Tuple exercise typed) ->
              let 
                total = String.length exercise.text
                typed' = String.length typed
                progress = if total == 0 then 0.0 else (toNumber typed' * 100.0) / toNumber total
                
                compareChars = foldlWithIndex 
                  (\i acc targetChar -> 
                    case String.codePointAt i typed of
                      Just c -> if c == targetChar then acc + 1 else acc
                      Nothing -> acc
                  )
                  0
                  (toCodePointArray exercise.text)
                
                accuracy = if typed' == 0 then 100.0 
                          else (toNumber compareChars / toNumber typed') * 100.0
              in
                D.div_ [
                  D.div_ [ text_ ("Progress: " <> show (min 100.0 (truncate progress)) <> "%") ],
                  D.div_ [ text_ ("Accuracy: " <> show (truncate accuracy) <> "%") ]
                ]
          ],     
        
        -- Exercise description
        D.div [ DA.klass_ "exercise-description" ] 
          [ text (("Exercise: " <> _) <<< _.description <$> currentExercise) ],
        
        -- Progress indicator
        (Tuple <$> exercises <*> currentExerciseIndex) <#~> \(Tuple exs idx) ->
          D.div [ DA.klass_ "progress-indicator" ]
            [ text_ ("Page " <> show (idx + 1) <> " of " <> show (length exs)) ],
        
        -- Text display section with reactive content
        D.div 
          [ DA.klass_ "text-display" ] 
          [
            (Tuple <$> currentExercise <*> typedInput) <#~> \(Tuple exercise typed) ->
              let 
                targetChars = toCodePointArray exercise.text
              in
                D.div_ (renderText targetChars typed)
          ],
        
        -- Hidden input - improved with autofocus and scroll prevention
        D.textarea
          [ DA.klass_ "hidden-input"
          , DA.id_ "typing-input"  
          , DA.style_ "position: fixed; left: -9999px; width: 1px; height: 1px; opacity: 0;"
          , DA.placeholder_ "Start typing..."
          , DA.autofocus_ "autofocus"
          , DL.keydown_ handleKeyPress
          ]
          [],
        
                -- Load Book button
        D.div 
          [ DA.klass_ "load-book-section" ] 
          [
            D.button
              [ DA.klass_ "load-book-btn"
              , DL.click_ $ const $ do
                launchAff_ do
                  result <- loadTextFile "books/TheDispossessed_LeGuin.txt"
                  liftEffect $ case result of
                    Left err -> pure unit  -- Could add error handling here
                    Right content -> do
                      let bookExercises = splitIntoPages content
                      when (length bookExercises > 0) do
                        setExercises bookExercises
                        setCurrentExerciseIndex 0
            ]
            [ text_ "Load Book" ]
        ],
        -- Controls section 
        D.div [ DA.klass_ "controls" ]
          [ D.button
              [ DA.klass_ "reset-btn"
              , DL.click_ $ const $ do
                  liftEffect $ setTypedInput ""
                  -- Also focus input when reset
                  liftEffect $ focusElementById "typing-input"
              ]
              [ text_ "Reset" ],
            
            D.button
              [ DA.klass_ "next-btn"
              , DL.click_ $ const nextExercise  
              ]
              [ text_ "Next Page" ]
          ]
      ]


-- Helper to truncate decimal places
truncate :: Number -> Number
truncate n = toNumber (floor (n * 10.0)) / 10.0

-- Function to render each character with the appropriate color
renderText :: Array CodePoint -> String -> Array Nut
renderText targetChars typedInput =
  let
    typedChars = toCodePointArray typedInput
  in
    mapWithIndex 
      (\i targetChar ->
        let
          charState = getCharState i targetChar typedChars
          charClass = case charState of
            NotTyped -> "char-not-typed"
            Correct -> "char-correct"
            Incorrect -> "char-incorrect"
        in
          D.span
            [ DA.klass_ charClass ]
            [ text_ (singleton targetChar) ]
      )
      targetChars

-- Function to determine the state of each character
getCharState :: Int -> CodePoint -> Array CodePoint -> CharState
getCharState index targetChar typedChars =
  case typedChars !! index of
    Nothing -> NotTyped
    Just typedChar ->
      if typedChar == targetChar
        then Correct
        else Incorrect