-- ==============================================================================
-- Nelson DMG setup AppleScript
-- This script is executed by CPack DragNDrop to configure the DMG window.
-- It sets up a polished installer look with a background image.
-- ==============================================================================
on run argv
  set diskName to item 1 of argv
  tell application "Finder"
    tell disk diskName
      open
      -- Set icon view with clean layout
      set current view of container window to icon view
      set toolbar visible of container window to false
      set statusbar visible of container window to false
      -- Window size: 660×400 — sized to show background nicely
      set the bounds of container window to {200, 120, 860, 520}
      set viewOptions to icon view options of container window
      set arrangement of viewOptions to not arranged
      set icon size of viewOptions to 96
      set text size of viewOptions to 13
      -- Set background image (copied by CPack into .background/)
      try
        set background picture of viewOptions to file ".background:background.png"
      end try
      -- Position icons: app left, Applications right, vertically centered
      set position of item "Nelson.app" of container window to {160, 220}
      set position of item "Applications" of container window to {500, 220}
      close
      open
      update without registering applications
      delay 2
    end tell
  end tell
end run
