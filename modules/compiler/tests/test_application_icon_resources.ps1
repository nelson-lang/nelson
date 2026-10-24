# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([Parameter(Mandatory)][string]$Executable, [Parameter(Mandatory)][string]$OutputDirectory)
$ErrorActionPreference = 'Stop'
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class IconResources {
 [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
 public static extern IntPtr LoadLibraryEx(string path, IntPtr file, uint flags);
 [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
 public static extern IntPtr FindResource(IntPtr module, IntPtr name, IntPtr type);
 [DllImport("kernel32.dll")] public static extern uint SizeofResource(IntPtr module, IntPtr resource);
 [DllImport("kernel32.dll")] public static extern IntPtr LoadResource(IntPtr module, IntPtr resource);
 [DllImport("kernel32.dll")] public static extern IntPtr LockResource(IntPtr resource);
 [DllImport("kernel32.dll")] public static extern bool FreeLibrary(IntPtr module);
 [DllImport("shell32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
 public static extern uint ExtractIconEx(string file, int index, [Out] IntPtr[] large, [Out] IntPtr[] small, uint count);
 [DllImport("user32.dll")] public static extern bool DestroyIcon(IntPtr icon);
}
'@
function Read-Resource([IntPtr]$Module, [int]$Type, [int]$Id) {
 $resource = [IconResources]::FindResource($Module, [IntPtr]$Id, [IntPtr]$Type)
 if ($resource -eq [IntPtr]::Zero) { throw "Missing resource $Type/$Id" }
 $size = [IconResources]::SizeofResource($Module, $resource)
 $pointer = [IconResources]::LockResource([IconResources]::LoadResource($Module, $resource))
 $bytes = [byte[]]::new($size)
 [Runtime.InteropServices.Marshal]::Copy($pointer, $bytes, 0, $size)
 return ,$bytes
}
New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null
$module = [IconResources]::LoadLibraryEx($Executable, [IntPtr]::Zero, 0x22)
if ($module -eq [IntPtr]::Zero) { throw 'Cannot read executable resources.' }
try {
 $group = Read-Resource $module 14 1
 if ($group.Length -ne 104 -or [BitConverter]::ToUInt16($group, 2) -ne 1 -or [BitConverter]::ToUInt16($group, 4) -ne 7) { throw 'Invalid icon group.' }
 $sizes = @(16, 24, 32, 48, 64, 128, 256)
 for ($k = 0; $k -lt $sizes.Count; $k++) {
  $offset = 6 + 14 * $k
  $side = $sizes[$k]
  if ($group[$offset] -ne ($side % 256) -or $group[$offset + 1] -ne ($side % 256)) { throw 'Wrong icon dimensions.' }
  if ([BitConverter]::ToUInt16($group, $offset + 6) -ne 32) { throw 'Wrong icon depth.' }
  $bytes = Read-Resource $module 3 ([BitConverter]::ToUInt16($group, $offset + 12))
  if ($bytes.Length -ne [BitConverter]::ToUInt32($group, $offset + 8)) { throw 'Wrong icon image size.' }
  [IO.File]::WriteAllBytes((Join-Path $OutputDirectory "icon-$side.png"), $bytes)
 }
 $manifest = Read-Resource $module 24 1
 if ([Text.Encoding]::UTF8.GetString($manifest) -notmatch 'longPathAware') { throw 'Launcher manifest was lost.' }
 $null = Read-Resource $module 16 1
} finally { $null = [IconResources]::FreeLibrary($module) }
$large = [IntPtr[]]::new(1)
$small = [IntPtr[]]::new(1)
try {
 $count = [IconResources]::ExtractIconEx($Executable, 0, $large, $small, 1)
 if ($count -eq 0 -or $count -eq [uint32]::MaxValue -or $large[0] -eq [IntPtr]::Zero -or $small[0] -eq [IntPtr]::Zero) {
  throw "Shell icon extraction failed: count=$count; nativeError=$([Runtime.InteropServices.Marshal]::GetLastWin32Error())"
 }
 Add-Type -AssemblyName System.Drawing
 $icon = [Drawing.Icon]::FromHandle($large[0])
 $bitmap = $icon.ToBitmap()
 try { $bitmap.Save((Join-Path $OutputDirectory 'shell-icon.png'), [Drawing.Imaging.ImageFormat]::Png) }
 finally { $bitmap.Dispose(); $icon.Dispose() }
} finally {
 if ($large[0] -ne [IntPtr]::Zero) { $null = [IconResources]::DestroyIcon($large[0]) }
 if ($small[0] -ne [IntPtr]::Zero) { $null = [IconResources]::DestroyIcon($small[0]) }
}
'ICON_RESOURCES_OK'
