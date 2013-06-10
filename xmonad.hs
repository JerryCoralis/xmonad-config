---------------------------------------------------------------------------
-- TB Schardl (neboat)
-- 06/2013
--
-- XMonad configuration.
--
-- This configuration is designed to work with Gnome.  It includes key
-- bindings for primarily browsing busy (i.e. nonempty) workspaces.
--
---------------------------------------------------------------------------
import XMonad                hiding( (|||) )
import XMonad.Util.Run
import XMonad.Util.EZConfig  ( additionalKeys )
import XMonad.Util.Font
import XMonad.Util.WorkspaceCompare

import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Layout.NoBorders     ( smartBorders )
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid          ( Grid(..) )
import XMonad.Layout.Magnifier     ( magnifiercz )
import XMonad.Layout.PerWorkspace  ( onWorkspace )
import XMonad.Layout.IM
import XMonad.Layout.Reflect       ( reflectHoriz )
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.Spiral
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.PerWorkspace

import XMonad.Prompt
import XMonad.Prompt.Shell  hiding ( getShellCompl )
import Data.Char            ( toLower )
import Data.List

import Codec.Binary.UTF8.String  ( encodeString, decodeString )
import System.Posix.Files        ( getFileStatus, isDirectory )

import XMonad.Config.Gnome
import XMonad.Config.Desktop  ( desktopLayoutModifiers )

import System.IO
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Control.Monad ( liftM2, unless )
import Data.Maybe    ( isJust )


---------------------------------------------------------------------------
-- Basic setup.
---------------------------------------------------------------------------
-- Use Super as mod
myModMask = mod4Mask

-- Set focused border color
myFocusedBorderColor = "#00adeb"

-- Set up 12 workspaces, where workspaces 1 and 12 are "special"
myWorkspaces = map show [1..12]
mySpecialWS  = ["1", "12"]

---------------------------------------------------------------------------
-- Management Hook
---------------------------------------------------------------------------
myManageHook = composeAll (
  [ manageHook gnomeConfig
  , className =? "Unity-2d-panel" --> doIgnore
  , className =? "Unity-2d-launcher" --> doFloat
  , className =? "Gimp" --> doFloat
  , className =? "Update-manager" --> doFloat
  , className =? "Skype" --> doFloat
  ])


---------------------------------------------------------------------------
-- Layouts
---------------------------------------------------------------------------
goldenRatio = toRational (2/(1 + sqrt 5 :: Double)) -- golden, thx Octoploid

-- My preferred layout for working in Emacs.
emacsDevLayout     = renamed [Replace "emacsDev"] $ Mirror $ ResizableTall 2 (3/100) 0.85 []
spiralLayout       = spiral goldenRatio
defaultTallLayout  = Mirror $ ResizableTall 1 (3/100) (1/2) []
simpleTabbedLayout = simpleTabbed

myLayouts   = emacsDevLayout ||| spiralLayout ||| defaultTallLayout ||| simpleTabbedLayout
-- Custom rotations of myLayouts for special workspaces
ws1Layouts  = spiralLayout ||| defaultTallLayout ||| simpleTabbedLayout ||| emacsDevLayout
ws12Layouts = defaultTallLayout ||| simpleTabbedLayout ||| emacsDevLayout ||| spiralLayout

-- myLayouts = renamed [Replace "emacsDev"] emacsDev |||
--             spiral goldenRatio                    |||
--             defaultTall                           |||
--             simpleTabbed
--   where
--     emacsDev    = Mirror $ ResizableTall 2 delta 0.85 []      -- My preferred layout for working in Emacs.
--     tiled       = ResizableTall nmaster delta goldenRatio []
--     defaultTall = Mirror $ ResizableTall nmaster delta (1/2) []
--     nmaster     = 1
--     delta       = 0.03


---------------------------------------------------------------------------
-- Additional key bindings
---------------------------------------------------------------------------
    
-- Helper function to skip empty workspaces
skipEmpty :: (Eq i) => [W.Workspace i l a] -> [W.Workspace i l a]
skipEmpty wss = filter (isJust . W.stack) wss

-- Helper function to first shift a window to another workspace and
-- then follow it.
shiftAndFollow :: WorkspaceId -> X()
shiftAndFollow = liftM2 (>>) (windows . W.shift) (windows . W.greedyView)

busyNotSpecial' :: [WorkspaceId] -> X (WindowSpace -> Bool)
busyNotSpecial' ids = return (\ws -> (isJust . W.stack) ws && ((`notElem` ids) . W.tag) ws)

-- Additional key bindings
myKeyBindings =
  [
    --  ((myModMask, xK_p), spawn "exe=`dmenu_path | dmenu -i` && eval \"exec $exe\"")
    ((myModMask, xK_p), myShellPrompt defaultXPConfig { position=Top })
    
    --, ((myModMask .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))
  , ((myModMask,               xK_Escape), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
  , ((myModMask .|. shiftMask, xK_Escape), spawn "gnome-session-quit")
    
    -- changing sizes
  , ((myModMask .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((myModMask .|. shiftMask, xK_l), sendMessage MirrorExpand)
    
    -- cycling through windows
  , ((myModMask,               xK_j), windows W.focusUp)   -- %! Move focus to the next window
  , ((myModMask,               xK_k), windows W.focusDown) -- %! Move focus to the previous window
  , ((myModMask .|. shiftMask, xK_j), windows W.swapUp)    -- %! Swap the focused window with the next window
  , ((myModMask .|. shiftMask, xK_k), windows W.swapDown)  -- %! Swap the focused window with the previous window
    
    -- cycling through workspaces
  , ((myModMask,               xK_Right), nextWS)
  , ((myModMask,               xK_Left),  prevWS)
  , ((myModMask,               xK_Down),  nextScreen)
  , ((myModMask,               xK_Up),    prevScreen)
  , ((myModMask .|. shiftMask, xK_Down),  shiftNextScreen)
  , ((myModMask .|. shiftMask, xK_Up),    shiftPrevScreen)
    
    --, ((myModMask,               xK_grave), toggleWS' mySpecialWS) -- toggle not-special workspaces
  , ((myModMask,               xK_grave), do                  -- toggle busy not-special workspaces
        hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
        unless (null hs') (windows . W.greedyView . W.tag $ head hs'))
  , ((myModMask,               xK_q), moveTo Next EmptyWS)    -- find next empty workspace
  , ((myModMask,               xK_s), moveTo Next (WSIs $ busyNotSpecial' mySpecialWS)) -- find next busy not-special workspace
  , ((myModMask,               xK_a), moveTo Prev (WSIs $ busyNotSpecial' mySpecialWS)) -- find prev busy not-special workspace
  , ((myModMask,               xK_bracketright), moveTo Next NonEmptyWS) -- find next busy workspace
  , ((myModMask,               xK_bracketleft), moveTo Prev NonEmptyWS) -- find prev busy workspace
  , ((myModMask .|. shiftMask, xK_s), shiftToNext >> nextWS)  -- shift to next workspace and follow
  , ((myModMask .|. shiftMask, xK_a), shiftToPrev >> prevWS)  -- shift to prev workspace and follow 
  , ((myModMask .|. shiftMask, xK_bracketright), shiftTo Next EmptyWS)   -- shift to next empty workspace
  , ((myModMask .|. shiftMask, xK_bracketleft), shiftTo Prev EmptyWS)   -- shift to next empty workspace
  , ((myModMask .|. shiftMask, xK_q),                         -- shift to next empty workspace and follow
     doTo Next EmptyWS getSortByIndex shiftAndFollow)
  , ((myModMask .|. shiftMask, xK_grave), do                  -- shift to last busy workspace and follow
        -- hs' <- gets (W.hidden . windowset)
        hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
        unless (null hs') (shiftAndFollow . W.tag $ head hs'))
    
    -- , ((myModMask .|. shiftMask, xK_w),                           -- shift to next empty workspace and follow
    --    doTo Next EmptyWS getSortByIndex
    --    $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))
    
    -- , ((myModMask .|. shiftMask, xK_a),
    --    doTo Prev EmptyWS getSortByIndex
    --    $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))  -- shift to next empty workspace and follow
    
    -- changing layouts 
  , ((myModMask,               xK_F1), sendMessage $ JumpToLayout "emacsDev")
  , ((myModMask,               xK_F2), sendMessage $ JumpToLayout "Spiral")
  , ((myModMask,               xK_F3), sendMessage $ JumpToLayout "Tabbed Simplest")
  , ((myModMask,               xK_F4), sendMessage $ JumpToLayout "Mirror ResizableTall")
    
    -- show help with keybindings
  , ((myModMask .|. shiftMask, xK_slash), spawn ("echo \"" ++ myHelp ++ "\" | xmessage -file -"))
  ]

myKeys = myKeyBindings ++ 
  [
    ((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

myHelp :: String
myHelp = unlines ["The modifier key is 'Super'.  Keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch gnome-terminal",
    "mod-p            Launch Shell Prompt",
    "mod-Shift-p      None (Standard: Launch gmrun)",
    "mod-Shift-c      Close/kill the focused window",
    "",
    "-- switching layouts",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-F1           Switch to emacsDev layout",
    "mod-F2           Switch to Spiral layout",
    "mod-F3           Switch to Tabbed Simplest layout",
    "mod-F4           Switch to Mirror ResizableTall layout",
    "mod-n            None (Standard: Resize/refresh viewed windows to the correct size)",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h        Shrink the master area",
    "mod-l        Expand the master area",
    "mod-Shift-h  Shrink the Mirror master area",
    "mod-Shift-l  Expand the Mirror master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-Escape  Quit xmonad",
    "mod-Escape        Restart xmonad",
    "",
    "-- Workspaces & screens (Workspaces 1 and 12 are special)",
    "mod-{[1..9],0,-,=}              Switch to Workspace [1..12]",
    "mod-Shift-{[1..9],0,-,=}        Move client to Workspace [1..12]",
    "mod-Right                       Switch to next Workspace",
    "mod-Left                        Switch to previous Workspace",
    "mod-bracketright                Switch to next nonempty Workspace",
    "mod-bracketleft                 Switch to previous nonempty Workspace",
    "mod-s                           Switch to next nonspecial nonempty Workspace",
    "mod-a                           Switch to previous nonspecial nonempty Workspace",
    "mod-Shift-s                     Move client and switch to next Workspace",
    "mod-Shift-a                     Move client and switch to previous Workspace",
    "mod-Shift-bracketright          Move client to next nonempty Workspace",
    "mod-Shift-bracketleft           Move client to previous nonempty Workspace",
    "mod-q                           Switch to next empty Workspace",
    "mod-Shift-q                     Move client and switch to next empty Workspace",
    "mod-grave       (mod-`)         Switch to last viewed nonspecial nonempty Workspace",
    "mod-Shift-grave (mod-Shift-`)   Move client and switch to last viewed nonspecial nonempty Workspace",
    "",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "mod-Down           Switch to next screen",
    "mod-Up             Switch to previous screen",
    "mod-Shift-Down     Move client to next screen",
    "mod-Shift-Up       Move client to previous screen",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging",
    "",
    "-- Help",
    "mod-question  Show this help message"]

---------------------------------------------------------------------------
-- Tweak Shell Prompt to search for infixes.
--
-- Taken from patch discussion for XMonad issue 393:
-- "XMonad.Prompt.Shell should use user-defined searchPredicate"
-- (https://code.google.com/p/xmonad/issues/detail?id=393)
---------------------------------------------------------------------------
myShellPrompt :: XPConfig -> X ()
myShellPrompt c = do
    cmds <- io $ getCommands
    mkXPrompt Shell c (getShellCompl cmds) spawnHere

getShellCompl :: [String] -> String -> IO [String]
getShellCompl cmds s | s == "" || last s == ' ' = return []
                     | otherwise                = do
    f     <- fmap lines $ runProcessWithInput "bash" []
             ("bind 'set completion-ignore-case on'; compgen -A file " ++ encodeString s ++ "\n")
    files <- case f of
               [x] -> do fs <- getFileStatus x
                         if isDirectory fs then return [x ++ "/"]
                                           else return [x]
               _   -> return f
    return . sortBy typedFirst . uniqSort $ files ++ commandCompletionFunction cmds s
    where
    typedFirst x y
        | x `startsWith` s && not (y `startsWith` s) = LT
        | y `startsWith` s && not (x `startsWith` s) = GT
        | otherwise = x `compare` y
    startsWith s ps = isPrefixOf (map toLower ps) (map toLower s)

commandCompletionFunction :: [String] -> String -> [String]
commandCompletionFunction cmds str | '/' `elem` str = []
                                   | otherwise = filter ((\x y -> map toLower x `isInfixOf` map toLower y) str) cmds


---------------------------------------------------------------------------
-- Xmobar configuration
--
-- More configuration for xmobar specified in xmobarrc.
---------------------------------------------------------------------------
myTitleColor  = "#eeeeee"
myTitleLength = 120
myCurrentWSColor = "#e6744c"
myVisibleWSColor = "#c185a7"
myUrgentWSColor  = "#cc0000"


---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------
main = do
xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
xmonad $ gnomeConfig {
    modMask            = myModMask
  , focusedBorderColor = myFocusedBorderColor
  , workspaces         = myWorkspaces
    -- Need to run Gnome startup hook to register Xmonad properly.
  , startupHook = do
      startupHook gnomeConfig
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageDocks <+> myManageHook
  , layoutHook = avoidStruts $ smartBorders $ 
                 onWorkspace "1" ws1Layouts $
                 onWorkspace "12" ws12Layouts $
                 desktopLayoutModifiers (myLayouts)
  , logHook = dynamicLogWithPP $ xmobarPP
              { ppOutput = hPutStrLn xmproc 
              , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
              , ppCurrent = xmobarColor myCurrentWSColor ""
              , ppVisible = xmobarColor myVisibleWSColor ""
              , ppUrgent  = xmobarColor myUrgentWSColor "" }
  } `additionalKeys` myKeys
