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


myModMask            = mod4Mask
myFocusedBorderColor = "#00adeb"
myWorkspaces = map show [1..12]
mySpecialWS = ["1", "12"]

myManageHook = composeAll (
  [ manageHook gnomeConfig
  , className =? "Unity-2d-panel" --> doIgnore
  , className =? "Unity-2d-launcher" --> doFloat
  , className =? "Gimp" --> doFloat
  , className =? "Update-manager" --> doFloat
  , className =? "Skype" --> doFloat
  ])

myLayouts = defaultTall      |||
            renamed [Replace "emacsDev"] emacsDev |||
            spiral ratio     |||
            Full             |||
            simpleTabbed     |||            
            Mirror tiled     
  where
    emacsDev    = Mirror $ ResizableTall 2 delta 0.85 []
    tiled       = ResizableTall nmaster delta ratio []
    defaultTall = ResizableTall nmaster delta (1/2) []
    nmaster     = 1
    ratio       = toRational (2/(1 + sqrt 5 :: Double)) -- golden, thx Octoploid
    delta       = 0.03

skipEmpty :: (Eq i) => [W.Workspace i l a] -> [W.Workspace i l a]
skipEmpty wss = filter (isJust . W.stack) wss

-- Bonus key bindings
myKeyBindings =
  [
  --  ((myModMask, xK_p), spawn "exe=`dmenu_path | dmenu -i` && eval \"exec $exe\"")
    ((myModMask, xK_p), myShellPrompt defaultXPConfig { position=Top })
    
  --, ((myModMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  , ((myModMask .|. shiftMask, xK_q), spawn "gnome-session-quit")
    
    -- changing sizes
  , ((myModMask .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((myModMask .|. shiftMask, xK_l), sendMessage MirrorExpand)
  --, ((myModMask, xK_BackSpace), spawn "~/.xmonad/showKeysScript")
    
     -- CycleWS setup
   , ((myModMask,               xK_Right), nextWS)
   , ((myModMask,               xK_Left),  prevWS)
   , ((myModMask,               xK_Down),  nextScreen)
   , ((myModMask,               xK_Up),    prevScreen)
   , ((myModMask .|. shiftMask, xK_Down),  shiftNextScreen)
   , ((myModMask .|. shiftMask, xK_Up),    shiftPrevScreen)
   --, ((myModMask,               xK_grave), toggleWS' mySpecialWS) -- toggle workspaces, except for special workspaces
   , ((myModMask,               xK_grave), do                     -- toggle nonempty workspaces, except for special workspaces
         hs' <- gets $ (flip skipTags) mySpecialWS . skipEmpty . W.hidden . windowset
         unless (null hs') (windows . W.greedyView . W.tag $ head hs')
     )
   , ((myModMask,               xK_w), moveTo Next EmptyWS)  -- find next empty workspace
   , ((myModMask,               xK_e), moveTo Prev EmptyWS)  -- find prev empty workspace
   , ((myModMask,               xK_s), moveTo Next NonEmptyWS)  -- find next busy workspace
   , ((myModMask,               xK_a), moveTo Prev NonEmptyWS)  -- find prev busy workspace
   , ((myModMask .|. shiftMask, xK_s), shiftToNext >> nextWS)  -- shift to next workspace and follow
   , ((myModMask .|. shiftMask, xK_a), shiftToPrev >> prevWS)  -- shift to prev workspace and follow 
   , ((myModMask .|. shiftMask, xK_w),
      doTo Next EmptyWS getSortByIndex
      $ liftM2 (>>) (windows . W.shift) (windows . W.greedyView))  -- shift to next empty workspace and follow
   , ((myModMask .|. shiftMask, xK_grave), do                      -- shift to last workspace and follow
         hs' <- gets (W.hidden . windowset)
         unless (null hs') ((liftM2 (>>) (windows . W.shift) (windows . W.greedyView)) . W.tag
                            $ head hs'))
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

-- Tweak Shell Prompt to search for infixes.
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

-- xmobar configuration
myTitleColor  = "#eeeeee"
myTitleLength = 120
myCurrentWSColor = "#e6744c"
myVisibleWSColor = "#c185a7"
myUrgentWSColor  = "#cc0000"

main = do
xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
xmonad $ gnomeConfig {
    modMask            = myModMask
  , focusedBorderColor = myFocusedBorderColor
  , workspaces         = myWorkspaces
  -- Need to run Gnome startup hook to register Xmonad properly
  -- , startupHook        = do
  --     spawn "~/.xmonad/startup-hook"
  , manageHook = manageDocks <+> myManageHook
  , layoutHook = avoidStruts $
                 desktopLayoutModifiers (myLayouts)
  , logHook = dynamicLogWithPP $ xmobarPP
              { ppOutput = hPutStrLn xmproc 
              , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
              , ppCurrent = xmobarColor myCurrentWSColor ""
              , ppVisible = xmobarColor myVisibleWSColor ""
              , ppUrgent  = xmobarColor myUrgentWSColor "" }
  } `additionalKeys` myKeys