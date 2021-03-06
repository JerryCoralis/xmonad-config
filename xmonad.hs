---------------------------------------------------------------------------
-- TB Schardl (neboat)
-- 09/2013
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
import Data.Maybe    ( isNothing, isJust )


---------------------------------------------------------------------------
-- Basic setup.
---------------------------------------------------------------------------
-- Use Super as mod
myModMask = mod4Mask

-- Set focused border color
myFocusedBorderColor = "#00adeb"

-- Set up 23 workspaces, where workspaces 1 and 13--23 are "special"
myWorkspaces = map show [1..23]
mySpecialWS  = ["1"] ++ map show [13..23]

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
emacsDevLayout     = renamed [Replace "emacsDev"] $ Mirror $ ResizableTall 3 (3/100) 0.85 []
spiralLayout       = spiral goldenRatio
defaultTallLayout  = Mirror $ ResizableTall 1 (3/100) (1/2) []
simpleTabbedLayout = simpleTabbed

myLayouts   = emacsDevLayout ||| spiralLayout ||| defaultTallLayout ||| simpleTabbedLayout
-- Custom rotations of myLayouts for special workspaces
specialWSLayouts  = spiralLayout ||| emacsDevLayout ||| defaultTallLayout ||| simpleTabbedLayout


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

busyHiddenNotSpecial' :: [WorkspaceId] -> X (WindowSpace -> Bool)
-- busyHiddenNotSpecial' ids = return (\ws -> (isJust . W.stack) ws
--                                            && ((`notElem` ids) . W.tag) ws)
busyHiddenNotSpecial' ids = do ne <- return (isJust . W.stack)                         -- busy
                               hi <- do hs <- gets (map W.tag . W.hidden . windowset)  -- hidden
                                        return (\ws -> W.tag ws `elem` hs)
                               ns <- return ((`notElem` ids) . W.tag)                  -- not special
                               return (\ws -> ne ws && hi ws && ns ws)

hiddenEmptyWS :: X (WindowSpace -> Bool)
hiddenEmptyWS = do em <- return (isNothing . W.stack)                      -- empty
                   hi <- do hs <- gets (map W.tag . W.hidden . windowset)  -- hidden
                            return (\ws -> W.tag ws `elem` hs)
                   return (\ws -> em ws && hi ws)

myKeyBindings =
  [
    --  ((myModMask, xK_p), spawn "exe=`dmenu_path | dmenu -i` && eval \"exec $exe\"")
    ((myModMask, xK_p), myShellPrompt defaultXPConfig { position=Top })
    
  , ((myModMask,               xK_Escape), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
  , ((myModMask .|. shiftMask, xK_Escape), spawn "gnome-session-quit")
  --, ((myModMask .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))
    
  , ((mod1Mask .|. controlMask, xK_l), spawn "gnome-screensaver-command -l")
    ---------------------------------------------------------------------------
    -- changing sizes
  , ((myModMask .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((myModMask .|. shiftMask, xK_l), sendMessage MirrorExpand)
    
    ---------------------------------------------------------------------------
    -- cycling through windows
  , ((myModMask .|. shiftMask, xK_Tab), windows W.swapDown) -- %! Swap the focused window with the next window
  , ((myModMask,               xK_j), windows W.focusUp)    -- %! Move focus to the next window
  , ((myModMask,               xK_k), windows W.focusDown)  -- %! Move focus to the previous window
  , ((myModMask .|. shiftMask, xK_j), windows W.swapUp)     -- %! Swap the focused window with the next window
  , ((myModMask .|. shiftMask, xK_k), windows W.swapDown)   -- %! Swap the focused window with the previous window
  
    ---------------------------------------------------------------------------
    -- cycling through screens
  , ((myModMask,               xK_Down),  nextScreen)
  , ((myModMask,               xK_Up),    prevScreen)
  -- , ((myModMask .|. shiftMask, xK_Down),  shiftNextScreen)
  -- , ((myModMask .|. shiftMask, xK_Up),    shiftPrevScreen)
  , ((myModMask .|. shiftMask, xK_Down),  swapNextScreen)
  , ((myModMask .|. shiftMask, xK_Up),    swapPrevScreen)
  , ((myModMask,               xK_backslash), swapNextScreen >> moveTo Next EmptyWS)
    -- swap this workspace with the workspace on the next screen,
    -- then follow.
  , ((myModMask .|. shiftMask, xK_backslash),
     shiftTo Next (WSIs hiddenEmptyWS) >> prevScreen
     >> do
       hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
       unless (null hs') (windows . W.greedyView . W.tag $ head hs')
    )
    
    ---------------------------------------------------------------------------
    -- cycling through workspaces
  , ((myModMask,               xK_Right), nextWS)
  , ((myModMask,               xK_Left),  prevWS)
    --, ((myModMask,               xK_grave), toggleWS' mySpecialWS) -- toggle not-special workspaces
    -- toggle busy not-special workspaces
  , ((myModMask,               xK_grave), do
        hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
        unless (null hs') (windows . W.greedyView . W.tag $ head hs'))
  --, ((myModMask,               xK_q), moveTo Next EmptyWS)    -- find next empty workspace
    
    -- find next empty workspace
  , ((myModMask,               xK_q), moveTo Next (WSIs hiddenEmptyWS))
    -- find next busy not-special workspace
  , ((myModMask,               xK_s), moveTo Next (WSIs $ busyHiddenNotSpecial' mySpecialWS))
    -- find prev busy not-special workspace
  , ((myModMask,               xK_a), moveTo Prev (WSIs $ busyHiddenNotSpecial' mySpecialWS))
    -- find next busy workspace
  , ((myModMask,               xK_f), moveTo Next HiddenNonEmptyWS)
    -- find prev busy workspace
  , ((myModMask,               xK_d), moveTo Prev HiddenNonEmptyWS)
    -- shift to next workspace and follow
  , ((myModMask .|. shiftMask, xK_s), doTo Next HiddenWS getSortByIndex shiftAndFollow)
    -- shift to prev workspace and follow 
  , ((myModMask .|. shiftMask, xK_a), doTo Prev HiddenWS getSortByIndex shiftAndFollow)
    -- shift to next hidden empty workspace
  , ((myModMask .|. shiftMask, xK_f), shiftTo Next HiddenWS)
    -- shift to next hidden empty workspace
  , ((myModMask .|. shiftMask, xK_d), shiftTo Prev HiddenWS)
    -- shift to next empty workspace and follow
  , ((myModMask .|. shiftMask, xK_q),
     doTo Next (WSIs hiddenEmptyWS) getSortByIndex shiftAndFollow)
    -- shift to last busy workspace and follow
  , ((myModMask .|. shiftMask, xK_grave), do
        -- hs' <- gets (W.hidden . windowset)
        hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
        unless (null hs') (shiftAndFollow . W.tag $ head hs'))
    
    -- , ((myModMask .|. shiftMask, xK_w),                           -- shift to next empty workspace and follow
    --    doTo Next EmptyWS getSortByIndex
    --    $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))
    
    -- , ((myModMask .|. shiftMask, xK_a),
    --    doTo Prev EmptyWS getSortByIndex
    --    $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))  -- shift to next empty workspace and follow
    
    ---------------------------------------------------------------------------
    -- changing layouts 
  , ((myModMask,               xK_F1), sendMessage $ JumpToLayout "emacsDev")
  , ((myModMask,               xK_F2), sendMessage $ JumpToLayout "Spiral")
  , ((myModMask,               xK_F3), sendMessage $ JumpToLayout "Tabbed Simplest")
  , ((myModMask,               xK_F4), sendMessage $ JumpToLayout "Mirror ResizableTall")
    
    ---------------------------------------------------------------------------
    -- adjusting volume
  , ((myModMask,               xK_KP_Divide), spawn "amixer -q set Master 3%-")
  , ((myModMask,               xK_KP_Multiply), spawn "amixer -q set Master 3%+")
  , ((myModMask,               xK_KP_Subtract), spawn "amixer -D pulse -q set Master toggle")
    
    ---------------------------------------------------------------------------
    -- show help with keybindings
  , ((myModMask .|. shiftMask, xK_slash), spawn ("echo \"" ++ myHelp ++ "\" | xmessage -file -"))
  ]

myKeys = myKeyBindings ++ 
  [
    ((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]
              ++ [xK_KP_Insert, xK_KP_Delete, xK_KP_End, xK_KP_Down, xK_KP_Page_Down,
                  xK_KP_Left, xK_KP_Begin, xK_KP_Right, xK_KP_Home, xK_KP_Up, xK_KP_Page_Up]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [
    ((m .|. myModMask, k), sc >>= screenWorkspace >>= flip whenJust (windows . f))
  | (k, sc) <- zip [xK_bracketleft, xK_bracketright] [(screenBy (-1)),(screenBy 1)]
  , (f, m) <- [(W.view, 0), (W.greedyView, shiftMask)]
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
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return     Swap the focused window and the master window",
    "mod-Shift-Tab  Swap the focused window with the next window",
    "mod-Shift-j    Swap the focused window with the next window",
    "mod-Shift-k    Swap the focused window with the previous window",
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
    "-- Workspaces (Workspaces 1 and 12 are special)",
    "mod-{[1..9],0,-,=}              Switch to Workspace [1..12]",
    "mod-Shift-{[1..9],0,-,=}        Move client to Workspace [1..12]",
    "mod-Right                       Switch to next Workspace",
    "mod-Left                        Switch to previous Workspace",
    "mod-f                           Switch to next nonempty hidden Workspace",
    "mod-d                           Switch to previous nonempty hidden Workspace",
    "mod-s                           Switch to next nonspecial nonempty hidden Workspace",
    "mod-a                           Switch to previous nonspecial nonempty Workspace",
    "mod-Shift-s                     Move client and switch to next hidden Workspace",
    "mod-Shift-a                     Move client and switch to previous hidden Workspace",
    "mod-Shift-f                     Move client to next hidden Workspace",
    "mod-Shift-d                     Move client to previous hidden Workspace",
    "mod-q                           Switch to next empty hidden Workspace",
    "mod-Shift-q                     Move client and switch to next empty hidden Workspace",
    "mod-grave       (mod-`)         Switch to last viewed nonspecial nonempty hidden Workspace",
    "mod-Shift-grave (mod-Shift-`)   Move client and switch to last viewed nonspecial nonempty hidden Workspace",
    "",
    "-- Screens",
    "mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3",
    "mod-Down             Switch to next screen",
    "mod-Up               Switch to previous screen",
    "mod-Shift-Down       Swap workspace with next screen",
    "mod-Shift-Up         Swap workspace with previous screen",
    "mod-backslash        Switch to next screen",
    "mod-Shift-backslash  Swap workspace with next screen",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging",
    "",
    "-- Volume control",
    "mod-kp-divide    Decrease volume",
    "mod-kp-multiply  Increase Volume",
    "mod-kp-subtract  Mute",
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
-- myVisibleWSColor = "#c185a7"
myVisibleWSColor = "#3eb0b0"
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
                 onWorkspaces mySpecialWS specialWSLayouts $
                 desktopLayoutModifiers (myLayouts)
  , logHook = dynamicLogWithPP $ xmobarPP
              { ppOutput = hPutStrLn xmproc 
              , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
              , ppCurrent = xmobarColor myCurrentWSColor ""
              , ppVisible = xmobarColor myVisibleWSColor ""
              , ppUrgent  = xmobarColor myUrgentWSColor "" }
  } `additionalKeys` myKeys

