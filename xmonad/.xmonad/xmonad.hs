import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout
import qualified XMonad.StackSet as W
import System.IO

main :: IO ()
main = do
	xbindkeys <- spawnPipe "xbindkeys"
	forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
			safeSpawn "mkfifo" ["/tmp/" ++ file]
	xmonad $ def
		{ terminal    = "termite"
		, workspaces  = myWorkspaces
		, focusFollowsMouse = True
		, focusedBorderColor = "#b16286"
		, normalBorderColor = "404040"
		, borderWidth = 2
		, modMask     = mod4Mask
		, manageHook  = manageDocks <+> manageHook def
		, layoutHook  = myLayoutHook
		, handleEventHook = handleEventHook def <+> docksEventHook
		, logHook     = myLogHook
		, startupHook = myStartupHook
		}
		`additionalKeysP` myAdditionalKeysP

myWorkspaces :: [String]
myWorkspaces = [ "code", "web" ]

myStartupHook = do
	spawn "xautolock -time 5 -locker \"i3lock -i /usr/share/backgrounds/hanyujie.png -p default -n\" -notify 20 -notifier \"xset dpms forceoff\" &"
	spawn "feh --bg-fill /usr/share/backgrounds/hanyujie.png &"
	spawn "$HOME/.config/polybar/launch.sh"
	spawn "compton -b"

myAdditionalKeysP =
	[ ("M-r", spawn "rofi -show run")
	, ("M-w", spawn "rofi -show window")
	]

myLayoutHook = avoidStruts (Full ||| tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
