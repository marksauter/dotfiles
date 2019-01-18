import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import System.IO

main :: IO ()
main = do
	xbindkeys <- spawnPipe "xbindkeys"
	xmonad $ def
		{ terminal    = "termite"
		, workspaces  = myWorkspaces
		, focusFollowsMouse = True
		, focusedBorderColor = "#b16286"
		, normalBorderColor = "404040"
		, borderWidth = 2
		, modMask     = mod4Mask
		, manageHook  = manageDocks <+> manageHook def
		, layoutHook  = avoidStruts $ layoutHook def
		, handleEventHook = handleEventHook def <+> docksEventHook
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
