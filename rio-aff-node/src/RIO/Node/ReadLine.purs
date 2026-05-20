-- | RIO-flavoured wrappers around `Node.ReadLine` and
-- | `Node.ReadLine.Aff`.
-- |
-- | A readline `Interface` is a value (a handle, similar to a
-- | `Buffer` or a `URL`), not a capability, so this module
-- | mirrors the API by lifting each `Effect`/`Aff` operation
-- | into `RIO`. The `closeH` / `lineH` / `historyH` / `pauseH` /
-- | `resumeH` / `sigContH` / `sigIntH` / `sigStpH` event handles
-- | are re-exported so callers can attach listeners via
-- | `RIO.Aff.Node.EventEmitter`.
module RIO.Aff.Node.ReadLine
  ( module Exports
  , CursorPos
  , KeySequenceObj
  , blockUntilClosed
  , clearEntireLine
  , clearEntireLine'
  , clearLineLeft
  , clearLineLeft'
  , clearLineRight
  , clearLineRight'
  , clearScreenDown
  , clearScreenDown'
  , close
  , countLines
  , createConsoleInterface
  , createInterface
  , cursor
  , cursorToX
  , cursorToX'
  , cursorToXY
  , cursorToXY'
  , emitKeyPressEvents
  , emitKeyPressEvents'
  , getCursorPos
  , getPrompt
  , line
  , moveCursorXY
  , moveCursorXY'
  , pause
  , prompt
  , prompt'
  , question
  , question'
  , resume
  , setPrompt
  , writeData
  , writeKey
  ) where

import Prelude

import Data.Options (Options)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Errors.AbortController (AbortController) as Exports
import Node.Errors.AbortController (AbortController)
import Node.ReadLine
  ( Completer
  , Interface
  , InterfaceOptions
  , closeH
  , completer
  , crlfDelay
  , escapeCodeTimeout
  , history
  , historyH
  , historySize
  , lineH
  , noCompletion
  , output
  , pauseH
  , promptStr
  , removeHistoryDuplicates
  , resumeH
  , sigContH
  , sigIntH
  , sigStpH
  , signal
  , tabSize
  , terminal
  , toEventEmitter
  ) as Exports
import Node.ReadLine (Completer, Interface, InterfaceOptions)
import Node.ReadLine as RL
import Node.ReadLine.Aff as RLAff
import Node.Stream (Readable, Writable)

import RIO.Aff.Core (RIO)

-- | Cursor position within the output stream.
type CursorPos =
  { rows :: Int
  , cols :: Int
  }

-- | A synthetic key sequence accepted by `writeKey`.
type KeySequenceObj =
  { name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

-- | Build an interface, attaching `input` and any options.
createInterface
  :: forall r e w
   . Readable w
  -> Options InterfaceOptions
  -> RIO r e Interface
createInterface input opts = liftEffect (RL.createInterface input opts)

-- | Build an interface wired to `process.stdin` / `process.stdout`
-- | with the given completion function.
createConsoleInterface :: forall r e. Completer -> RIO r e Interface
createConsoleInterface c = liftEffect (RL.createConsoleInterface c)

-- | Close the interface; emits the `close` event.
close :: forall r e. Interface -> RIO r e Unit
close iface = liftEffect (RL.close iface)

-- | Pause the input stream.
pause :: forall r e. Interface -> RIO r e Unit
pause iface = liftEffect (RL.pause iface)

-- | Resume the input stream.
resume :: forall r e. Interface -> RIO r e Unit
resume iface = liftEffect (RL.resume iface)

-- | Write the configured prompt to output and ask for input.
prompt :: forall r e. Interface -> RIO r e Unit
prompt iface = liftEffect (RL.prompt iface)

-- | `prompt` with an explicit `preserveCursor` flag.
prompt' :: forall r e. Boolean -> Interface -> RIO r e Unit
prompt' preserveCursor iface = liftEffect (RL.prompt' preserveCursor iface)

-- | Ask a question and block (in `RIO`) until the user replies.
-- | Built on `Node.ReadLine.Aff.question`; the underlying Node
-- | binding's callback-style `question` would require running an
-- | arbitrary `RIO r e` action from inside `Effect`, which is not
-- | something we can do without the surrounding env / error row,
-- | so only the `Aff`-style entry point is exposed.
question :: forall r e. String -> Interface -> RIO r e String
question txt iface = liftAff (RLAff.question txt iface)

-- | Cancellable `question` driven by an `AbortController`.
question'
  :: forall r e
   . String
  -> AbortController
  -> Interface
  -> RIO r e String
question' txt ctrl iface = liftAff (RLAff.question' txt ctrl iface)

-- | Block until the interface emits `close`.
blockUntilClosed :: forall r e. Interface -> RIO r e Unit
blockUntilClosed iface = liftAff (RLAff.blockUntilClosed iface)

-- | Count `line` events until the interface closes (or errors).
countLines :: forall r e. Interface -> RIO r e Int
countLines iface = liftAff (RLAff.countLines iface)

-- | Update the prompt string written by `prompt`.
setPrompt :: forall r e. String -> Interface -> RIO r e Unit
setPrompt newPrompt iface = liftEffect (RL.setPrompt newPrompt iface)

-- | Read back the current prompt string.
getPrompt :: forall r e. Interface -> RIO r e String
getPrompt iface = liftEffect (RL.getPrompt iface)

-- | Write a chunk of text to the interface as if the user had
-- | typed it.
writeData :: forall r e. String -> Interface -> RIO r e Unit
writeData s iface = liftEffect (RL.writeData s iface)

-- | Inject a synthetic key sequence into the interface.
writeKey :: forall r e. KeySequenceObj -> Interface -> RIO r e Unit
writeKey k iface = liftEffect (RL.writeKey k iface)

-- | Current input-line buffer.
line :: forall r e. Interface -> RIO r e String
line iface = liftEffect (RL.line iface)

-- | Cursor position within the input-line buffer.
cursor :: forall r e. Interface -> RIO r e Int
cursor iface = liftEffect (RL.cursor iface)

-- | `{ rows, cols }` cursor position within the output stream.
getCursorPos :: forall r e. Interface -> RIO r e CursorPos
getCursorPos iface = liftEffect (RL.getCursorPos iface)

-- | Clear from the cursor leftwards. Returns whether the write was
-- | fully flushed (the underlying Node binding returns a `Boolean`).
clearLineLeft :: forall r e w. Writable w -> RIO r e Boolean
clearLineLeft s = liftEffect (RL.clearLineLeft s)

-- | `clearLineLeft` with a flush-completed callback.
clearLineLeft'
  :: forall r e w
   . Writable w
  -> RIO r e Unit
  -> RIO r e Boolean
clearLineLeft' s _ = liftEffect (RL.clearLineLeft' s (pure unit))

-- | Clear from the cursor rightwards.
clearLineRight :: forall r e w. Writable w -> RIO r e Boolean
clearLineRight s = liftEffect (RL.clearLineRight s)

-- | `clearLineRight` with a flush-completed callback.
clearLineRight'
  :: forall r e w
   . Writable w
  -> RIO r e Unit
  -> RIO r e Boolean
clearLineRight' s _ = liftEffect (RL.clearLineRight' s (pure unit))

-- | Clear the entire current line.
clearEntireLine :: forall r e w. Writable w -> RIO r e Boolean
clearEntireLine s = liftEffect (RL.clearEntireLine s)

-- | `clearEntireLine` with a flush-completed callback.
clearEntireLine'
  :: forall r e w
   . Writable w
  -> RIO r e Unit
  -> RIO r e Boolean
clearEntireLine' s _ = liftEffect (RL.clearEntireLine' s (pure unit))

-- | Clear everything from the cursor downwards.
clearScreenDown :: forall r e w. Writable w -> RIO r e Boolean
clearScreenDown s = liftEffect (RL.clearScreenDown s)

-- | `clearScreenDown` with a flush-completed callback.
clearScreenDown'
  :: forall r e w
   . Writable w
  -> RIO r e Unit
  -> RIO r e Boolean
clearScreenDown' s _ = liftEffect (RL.clearScreenDown' s (pure unit))

-- | Move the cursor to column `x` on the current line.
cursorToX :: forall r e w. Writable w -> Int -> RIO r e Boolean
cursorToX s x = liftEffect (RL.cursorToX s x)

-- | `cursorToX` with a flush-completed callback.
cursorToX'
  :: forall r e w
   . Writable w
  -> Int
  -> RIO r e Unit
  -> RIO r e Boolean
cursorToX' s x _ = liftEffect (RL.cursorToX' s x (pure unit))

-- | Move the cursor to (`x`, `y`).
cursorToXY
  :: forall r e w
   . Writable w
  -> Int
  -> Int
  -> RIO r e Boolean
cursorToXY s x y = liftEffect (RL.cursorToXY s x y)

-- | `cursorToXY` with a flush-completed callback.
cursorToXY'
  :: forall r e w
   . Writable w
  -> Int
  -> Int
  -> RIO r e Unit
  -> RIO r e Boolean
cursorToXY' s x y _ = liftEffect (RL.cursorToXY' s x y (pure unit))

-- | Move the cursor relatively by (`dx`, `dy`).
moveCursorXY
  :: forall r e w
   . Writable w
  -> Int
  -> Int
  -> RIO r e Boolean
moveCursorXY s x y = liftEffect (RL.moveCursorXY s x y)

-- | `moveCursorXY` with a flush-completed callback.
moveCursorXY'
  :: forall r e w
   . Writable w
  -> Int
  -> Int
  -> RIO r e Unit
  -> RIO r e Boolean
moveCursorXY' s x y _ = liftEffect (RL.moveCursorXY' s x y (pure unit))

-- | Have the readable stream emit `keypress` events.
emitKeyPressEvents :: forall r e w. Readable w -> RIO r e Unit
emitKeyPressEvents s = liftEffect (RL.emitKeyPressEvents s)

-- | `emitKeyPressEvents` bound to a specific interface for
-- | input-handling.
emitKeyPressEvents'
  :: forall r e w
   . Readable w
  -> Interface
  -> RIO r e Unit
emitKeyPressEvents' s iface = liftEffect (RL.emitKeyPressEvents' s iface)
