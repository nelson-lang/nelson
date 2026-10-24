-- ==============================================================================
-- Nelson DMG setup AppleScript
-- This script is executed by CPack DragNDrop to configure the DMG window.
-- It sets up a polished installer look with a background image.
-- ==============================================================================
on run argv
  set diskName to item 1 of argv
  set volumePath to "/Volumes/" & diskName

  try
    do shell script "/usr/bin/SetFile -a C " & quoted form of volumePath
  on error errMsg
    log "Nelson DMG volume icon flag skipped: " & errMsg
  end try

  with timeout of 60 seconds
    tell application "Finder"
      try
        tell disk (diskName as string)
          open

          tell container window
            set current view to icon view
            set toolbar visible to false
            set statusbar visible to false
            set the bounds to {200, 120, 860, 520}
          end tell

          set viewOptions to the icon view options of container window
          tell viewOptions
            set arrangement to not arranged
            set icon size to 96
            set text size to 13
          end tell
          set background picture of viewOptions to file ".background:background.png"

          set position of item "Nelson.app" to {160, 220}
          set position of item "Applications" to {500, 220}

          close
          open
          delay 1

          tell container window
            set statusbar visible to false
            set the bounds to {200, 120, 860, 520}
          end tell

          update without registering applications
          delay 3
        end tell
      on error errMsg number errNum
        error "Nelson DMG Finder layout failed (" & errNum & "): " & errMsg & ". Grant Automation permission so the build tool can control Finder, or disable NELSON_DMG_FINDER_LAYOUT." number errNum
      end try
    end tell
  end timeout
end run
