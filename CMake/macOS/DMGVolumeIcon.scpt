-- ==============================================================================
-- Nelson DMG volume icon setup AppleScript
-- This script is executed by CPack while the temporary DMG volume is mounted.
-- ==============================================================================
on run argv
  set diskName to item 1 of argv
  set volumePath to "/Volumes/" & diskName
  do shell script "/usr/bin/SetFile -a C " & quoted form of volumePath
end run
