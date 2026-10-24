-- ==============================================================================
-- Nelson DMG file icon setup AppleScript
-- This script is executed by CPack after the DMG file is generated.
-- ==============================================================================
use framework "AppKit"
use scripting additions

on run argv
  set iconPath to item 1 of argv
  set dmgPath to item 2 of argv
  set iconImage to current application's NSImage's alloc()'s initWithContentsOfFile:iconPath
  if iconImage = missing value then error "Cannot load icon: " & iconPath
  set ok to current application's NSWorkspace's sharedWorkspace()'s setIcon:iconImage forFile:dmgPath options:0
  if (ok as boolean) is false then error "Cannot set icon: " & dmgPath
  do shell script "/usr/bin/SetFile -a C " & quoted form of dmgPath
end run
