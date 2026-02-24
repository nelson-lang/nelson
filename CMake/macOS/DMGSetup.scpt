-- ==============================================================================
-- Nelson DMG setup AppleScript
-- This script is executed by CPack DragNDrop to configure the DMG window.
-- ==============================================================================
on run argv
  tell application "Finder"
    tell disk (item 1 of argv)
      open
      set current view of container window to icon view
      set toolbar visible of container window to false
      set statusbar visible of container window to false
      set the bounds of container window to {200, 120, 760, 500}
      set viewOptions to icon view options of container window
      set arrangement of viewOptions to not arranged
      set icon size of viewOptions to 80
      -- Position the application bundle and the Applications symlink
      set position of item "Nelson.app" of container window to {140, 200}
      set position of item "Applications" of container window to {420, 200}
      close
      open
      update without registering applications
      delay 2
    end tell
  end tell
end run
