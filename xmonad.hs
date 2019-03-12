-- Xmonad configuration
-- To reload: mod-q

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import System.IO
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Accordion
import XMonad.Layout.Dishes
import XMonad.Layout.Named
import XMonad.Layout.DragPane
import XMonad.Util.Paste
import XMonad.Util.Themes
import XMonad.Prompt
import XMonad.Prompt.Ssh
--import Xmonad.Prompt.Pass
import XMonad.Actions.Submap
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.StackSet (shiftMaster)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

my_tabbed = tabbed shrinkText (theme smallClean)
my_split_tabs = named "Split Tab" $ combineTwo (TwoPane (3/100) (1/2)) my_tabbed my_tabbed
my_tall = Tall 1 (3/100) (3/4)
--my_half_split = Accordion
--my_half_split_tabs = named "Half Tab" $ combineTwo (TwoPane (3/100) (1/2)) 
--    my_tabbed my_half_split

myLayoutHook = avoidStruts $ desktopLayoutModifiers $ windowNavigation (my_split_tabs ||| my_tall ||| Full)

myKeys x = [
    -- Navigation
      ((modMask x, xK_k), sendMessage $ Go U)
    , ((modMask x, xK_j), sendMessage $ Go D)
    , ((modMask x, xK_l), sendMessage $ Go R)
    , ((modMask x, xK_h), sendMessage $ Go L)
    , ((modMask x .|. shiftMask, xK_k), sendMessage $ Move U)
    , ((modMask x .|. shiftMask, xK_j), sendMessage $ Move D)
    , ((modMask x .|. shiftMask, xK_l), sendMessage $ Move R)
    , ((modMask x .|. shiftMask, xK_h), sendMessage $ Move L)
    , ((modMask x, xK_i), windows W.focusDown)
    , ((modMask x, xK_u), windows W.focusUp)
    , ((modMask x .|. shiftMask, xK_i), windows W.swapDown)
    , ((modMask x .|. shiftMask, xK_u), windows W.swapUp)
    , ((modMask x .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask x, xK_Delete), kill)
    , ((modMask x, xK_BackSpace), kill)
    , ((modMask x, xK_comma), prevWS)
    , ((modMask x, xK_period), nextWS)
    , ((modMask x .|. shiftMask, xK_comma), shiftToPrev >> prevWS)
    , ((modMask x .|. shiftMask, xK_period), shiftToNext >> nextWS)
    -- Programs
    , ((modMask x, xK_Return), runInTerm "" "tmux -2 a")
    , ((modMask x .|. shiftMask, xK_Return), spawn $ XMonad.terminal x)
    , ((modMask x, xK_bracketleft), sshPrompt def)
    , ((modMask x, xK_bracketright), shellPromptHere def)
    , ((modMask x, xK_o), runInTerm "" "w3m https://duckduckgo.com")
    , ((modMask x, xK_semicolon), sendMessage NextLayout)
    , ((modMask x .|. shiftMask, xK_semicolon), windows W.shiftMaster)
    -- , ((modMask x, xK_colon), setLayout $ XMonad.layoutHook conf)
    , ((modMask x, xK_space), spawn "/usr/bin/emacsclient -c")
    , ((modMask x .|. shiftMask, xK_space), spawn "/usr/bin/emacs")
    -- Paste
    , ((modMask x, xK_y), submap . M.fromList $
                          [ ((0, xK_y), spawn "xdotool key --clearmodifiers 'Shift+Insert'")
                          ])
    -- Password lookup and generation with dmenu and lastpass
    -- , ((modMask x , xK_p), spawn "passmenu --type")
    -- , ((modMask x .|. shiftMask, xK_p), spawn "passmenu --type-login")
    , ((modMask x , xK_p), spawn "lastpass-dmenu --typeit")
    , ((modMask x .|. shiftMask, xK_p), spawn "lastpass-dmenu --typeit-login")
    --, ((modMask x .|. controlMask, xK_p), passGeneratePrompt xpconfig)
    -- Locks up urxvt
    --, ((modMask x, xK_y), submap . M.fromList $
    --                      [ ((0, xK_y), pasteSelection)
    --                      ])
    --, ((modMask x, xK_p), spawn "xsel | xvkbd -xsendevent -file -")
  ]
finalKeys x = M.union (M.fromList (myKeys x)) (keys gnomeConfig x) 

defaultModMask :: KeyMask
defaultModMask = mod4Mask

myWorkspaces    = ["web", "mail"] ++ map show [3 .. 7 :: Int] ++ ["az", "weechat"]

myManageHook = composeAll . concat $ [
  [className =? "Unity-2d-panel"    --> doIgnore]
  , [className =? "Unity-2d-launcher" --> doIgnore]
  , [className =? "Wfica" --> doShift "az"]
  ]

main = do
    -- External programs to run at start time
    spawn "unclutter"
    -- Start xmonad
    xmonad $ gnomeConfig
        { terminal = "urxvt -tn xterm-256color"
        , focusFollowsMouse = False
        , manageHook = manageSpawn <+> manageDocks <+> myManageHook <+> manageHook gnomeConfig
        , layoutHook = myLayoutHook
        , modMask = defaultModMask
        , keys = finalKeys
        , workspaces = myWorkspaces
        , borderWidth = 2
        , normalBorderColor  = "#0c2019"
        , focusedBorderColor = "#d5e0de"
        }
