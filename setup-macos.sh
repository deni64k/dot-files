#!/bin/sh

# https://github.com/herrbischoff/awesome-macos-command-line

reload_dock=0
reload_finder=0

if [ "$(defaults read -g AppleInterfaceStyle)" != "Dark" ]; then
  defaults write -g AppleInterfaceStyle -string Dark
  reload_dock=1
  reload_finder=1
fi

if [ "$(defaults read -g AppleHighlightColor)" != "1.000000 0.749020 0.823529 Pink" ]; then
  defaults write -g AppleHighlightColor -string "1.000000 0.749020 0.823529 Pink"
  reload_dock=1
  reload_finder=1
fi

if [ "$(defaults read -g NSRequiresAquaSystemAppearance)" -ne "1" ]; then
  defaults write -g NSRequiresAquaSystemAppearance -bool true
  reload_dock=1
fi

if [ "$(defaults read com.apple.Dock autohide)" -ne "1" ]; then
  defaults write com.apple.Dock autohide -bool true
  reload_dock=1
fi

if [ "$(defaults read com.apple.Dock "autohide-time-modifier")" != "0.5" ]; then
  defaults write com.apple.Dock "autohide-time-modifier" -float 0.5
  reload_dock=1
fi

if [ "$(defaults read com.apple.Dock "magnification")" -ne "1" ]; then
  defaults write com.apple.Dock "magnification" -bool 1
  reload_dock=1
fi

if [ "$(defaults read com.apple.Dock "tilesize")" -ne "40" ]; then
  defaults write com.apple.Dock "tilesize" -int 40
  reload_dock=1
fi

if [ "$(defaults read com.apple.Dock "largesize")" -ne "70" ]; then
  defaults write com.apple.Dock "largesize" -int 70
  reload_dock=1
fi

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Don’t show recent applications in Dock
defaults write com.apple.Dock show-recents -bool false

# Hot corners
# Possible values:
#  0: no-op
#  2: Mission Control
#  3: Show application windows
#  4: Desktop
#  5: Start screen saver
#  6: Disable screen saver
#  7: Dashboard
# 10: Put display to sleep
# 11: Launchpad
# 12: Notification Center
# 13: Lock Screen
# Top left screen corner → Lock Screen
defaults write com.apple.Dock wvous-tl-corner -int 13
defaults write com.apple.Dock wvous-tl-modifier -int 0

defaults write -g AppleLocale -string "en_US@currency=CAD"
defaults write -g AppleMeasurementUnits -string "Inches"
defaults write -g AppleMetricUnits -bool false
defaults write -g AppleTemperatureUnit -string "Fahrenheit"

# Enable full keyboard access for all controls
# (e.g. enable Tab in modal dialogs)
defaults write -g AppleKeyboardUIMode -int 3

# Show All File Extensions
defaults write -g AppleShowAllExtensions -bool true

# Show Full Path in Finder Title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Increase Number of Recent Places
defaults write -g NSNavRecentPlacesLimit -int 10

# Set sidebar icon size to medium
defaults write -g NSTableViewDefaultSizeMode -int 1

# Expand save panel by default
defaults write -g NSNavPanelExpandedStateForSaveMode -bool true
defaults write -g NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write -g PMPrintingExpandedStateForPrint -bool true
defaults write -g PMPrintingExpandedStateForPrint2 -bool true

# Scrollbar Visibility
# Possible values: WhenScrolling, Automatic and Always.
defaults write -g AppleShowScrollBars -string "WhenScrolling"

# Show Hidden Files
defaults write com.apple.finder AppleShowAllFiles -bool true

#
# Terminal.app
#
# Use a Dark Pastel theme by default in Terminal.app
theme="Dark Pastel"
osascript <<EOD
tell application "Terminal"
	local allOpenedWindows
	local initialOpenedWindows
	local windowID
	set themeName to "$theme"
	(* Store the IDs of all the open terminal windows. *)
	set initialOpenedWindows to id of every window
	(* Open the custom theme so that it gets added to the list
	   of available terminal themes (note: this will open two
	   additional terminal windows). *)
	do shell script "open './macos-terminal-themes-master/schemes/" & themeName & ".terminal'"
	(* Wait a little bit to ensure that the custom theme is added. *)
	delay 1
	(* Set the custom theme as the default terminal theme. *)
	set default settings to settings set themeName
	(* Get the IDs of all the currently opened terminal windows. *)
	set allOpenedWindows to id of every window
	repeat with windowID in allOpenedWindows
		(* Close the additional windows that were opened in order
		   to add the custom theme to the list of terminal themes. *)
		if initialOpenedWindows does not contain windowID then
			close (every window whose id is windowID)
		(* Change the theme for the initial opened terminal windows
		   to remove the need to close them in order for the custom
		   theme to be applied. *)
		else
			set current settings of tabs of (every window whose id is windowID) to settings set themeName
		end if
	end repeat
end tell
EOD
plutil -replace "Window Settings.Dark Pastel.useOptionAsMetaKey" -bool "true" \
       $HOME/Library/Preferences/com.apple.Terminal.plist

# Enable Develop Menu and Web Inspector
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true
defaults write -g WebKitDeveloperExtras -bool true

# Disable Creation of Metadata Files on Network Volumes
# Avoids creation of .DS_Store and AppleDouble files.
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Disable Creation of Metadata Files on USB Volumes
# Avoids creation of .DS_Store and AppleDouble files.
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

defaults write org.macosforge.xquartz.X11 enable_iglx -bool true

# Enable “Do Not Track”
defaults write com.apple.Safari SendDoNotTrackHTTPHeader -bool true

# Use `~/Documents/Torrents` to store incomplete downloads
defaults write org.m0k.transmission UseIncompleteDownloadFolder -bool true
defaults write org.m0k.transmission IncompleteDownloadFolder -string "${HOME}/Documents/Incomplete"

# Use `~/Downloads` to store completed downloads
defaults write org.m0k.transmission DownloadLocationConstant -bool true

# Don’t prompt for confirmation before downloading
defaults write org.m0k.transmission DownloadAsk -bool false
defaults write org.m0k.transmission MagnetOpenAsk -bool false

# Don’t prompt for confirmation before removing non-downloading active transfers
defaults write org.m0k.transmission CheckRemoveDownloading -bool true

# Trash original torrent files
defaults write org.m0k.transmission DeleteOriginalTorrent -bool true

# Hide the donate message
defaults write org.m0k.transmission WarningDonate -bool false
# Hide the legal disclaimer
defaults write org.m0k.transmission WarningLegal -bool false

# IP block list.
# Source: https://giuliomac.wordpress.com/2014/02/19/best-blocklist-for-transmission/
defaults write org.m0k.transmission BlocklistNew -bool true
defaults write org.m0k.transmission BlocklistURL -string "http://john.bitsurge.net/public/biglist.p2p.gz"
defaults write org.m0k.transmission BlocklistAutoUpdate -bool true

# Randomize port on launch
defaults write org.m0k.transmission RandomPort -bool true

# Kill affected apps
for app in "Dock" "Finder"; do
  killall "${app}" > /dev/null 2>&1
done

# Done
echo "Done. Note that some of these changes require a logout/restart to take effect."
