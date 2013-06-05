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

-- Focus does not follow mouse
myFocusFollowsMouse = False

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

myLayouts = renamed [Replace "emacsDev"] emacsDev |||
            defaultTall                           |||
            spiral goldenRatio                    |||
            Full                                  |||
            simpleTabbed                          |||            
            Mirror tiled     
  where
    emacsDev    = Mirror $ ResizableTall 2 delta 0.85 []      -- My preferred layout for working in Emacs.
    tiled       = ResizableTall nmaster delta goldenRatio []
    defaultTall = ResizableTall nmaster delta (1/2) []
    nmaster     = 1
    delta       = 0.03


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
    
    --, ((myModMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  , ((myModMask .|. shiftMask, xK_q), spawn "gnome-session-quit")
    
    -- changing sizes
  , ((myModMask .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((myModMask .|. shiftMask, xK_l), sendMessage MirrorExpand)
    
    -- , ((myModMask,               xK_BackSpace), spawn "~/.xmonad/showKeysScript")
    
    -- CycleWS setup
  , ((myModMask,               xK_Right), nextWS)
  , ((myModMask,               xK_Left),  prevWS)
  , ((myModMask,               xK_Down),  nextScreen)
  , ((myModMask,               xK_Up),    prevScreen)
  , ((myModMask .|. shiftMask, xK_Down),  shiftNextScreen)
  , ((myModMask .|. shiftMask, xK_Up),    shiftPrevScreen)
    --, ((myModMask,               xK_grave), toggleWS' mySpecialWS) -- toggle not-special workspaces
  , ((myModMask,               xK_grave), do                       -- toggle busy not-special workspaces
        hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
        unless (null hs') (windows . W.greedyView . W.tag $ head hs'))
  , ((myModMask,               xK_w), moveTo Next EmptyWS)         -- find next empty workspace
  , ((myModMask,               xK_e), moveTo Prev EmptyWS)         -- find prev empty workspace
  , ((myModMask,               xK_s), moveTo Next (WSIs $ busyNotSpecial' mySpecialWS)) -- find next busy not-special workspace
  , ((myModMask,               xK_a), moveTo Prev (WSIs $ busyNotSpecial' mySpecialWS)) -- find prev busy not-special workspace
  , ((myModMask,               xK_f), moveTo Next NonEmptyWS) -- find next busy not-special workspace
  , ((myModMask,               xK_d), moveTo Prev NonEmptyWS) -- find prev busy not-special workspace
  , ((myModMask .|. shiftMask, xK_s), shiftToNext >> nextWS)       -- shift to next workspace and follow
  , ((myModMask .|. shiftMask, xK_a), shiftToPrev >> prevWS)       -- shift to prev workspace and follow 
  , ((myModMask .|. shiftMask, xK_w),                              -- shift to next empty workspace and follow
     doTo Next EmptyWS getSortByIndex shiftAndFollow)
  , ((myModMask .|. shiftMask, xK_grave), do                       -- shift to last busy workspace and follow
        -- hs' <- gets (W.hidden . windowset)
        hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
        unless (null hs') (shiftAndFollow . W.tag $ head hs'))
    
    -- , ((myModMask .|. shiftMask, xK_w),                           -- shift to next empty workspace and follow
    --    doTo Next EmptyWS getSortByIndex
    --    $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))
    
    -- , ((myModMask .|. shiftMask, xK_a),
    --    doTo Prev EmptyWS getSortByIndex
    --    $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))  -- shift to next empty workspace and follow
    
    -- Jump to layouts 
  , ((myModMask,               xK_F1), sendMessage $ JumpToLayout "emacsDev")
  , ((myModMask,               xK_F2), sendMessage $ JumpToLayout "Spiral")
  ]

myKeys = myKeyBindings ++ 
  [
    ((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

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
  , focusFollowsMouse  = myFocusFollowsMouse
    -- Need to run Gnome startup hook to register Xmonad properly.
  , startupHook = do
      startupHook gnomeConfig
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageDocks <+> myManageHook
  , layoutHook = onWorkspace "1" (avoidStruts $ smartBorders (spiral goldenRatio)) $
                 onWorkspace "12" (avoidStruts $ smartBorders (ResizableTall 1 (3/100) (1/2) [])) $
                 avoidStruts $ smartBorders $ desktopLayoutModifiers (myLayouts)
  , logHook = dynamicLogWithPP $ xmobarPP
              { ppOutput = hPutStrLn xmproc 
              , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
              , ppCurrent = xmobarColor myCurrentWSColor ""
              , ppVisible = xmobarColor myVisibleWSColor ""
              , ppUrgent  = xmobarColor myUrgentWSColor "" }
  } `additionalKeys` myKeys
