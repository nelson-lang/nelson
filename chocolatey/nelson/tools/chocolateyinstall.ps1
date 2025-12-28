$ErrorActionPreference = 'Stop'

$packageName    = 'nelson'
$installerType  = 'exe'
$silentArgs     = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url64          = 'https://github.com/nelson-lang/nelson/releases/download/v1.16.0/Nelson-1.16.0.5575-x86-64.exe'
$checksum64     = '539e5dc2e357543733a1371ccaf3b2ca1d03b57fddc94ae9e66c7890850e02cc'
$checksumType64 = 'sha256'
$validExitCodes = @(0, 1638, 3010)

Install-ChocolateyPackage `
  -PackageName    $packageName `
  -FileType       $installerType `
  -SilentArgs     $silentArgs `
  -Url64bit       $url64 `
  -ValidExitCodes $validExitCodes `
  -Checksum64     $checksum64 `
  -ChecksumType64 $checksumType64
