$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.14.0/Nelson-1.14.0.5060-x86-64.exe'
$checksum64 = 'e2d712798646ea2b1aae8a2dc74b3cd8523644f3242eb756f73ee76b502ed7bb'
$checksumType64 = 'sha256'
$validExitCodes = @(0, 1638, 3010)

Install-ChocolateyPackage -PackageName "$packageName" `
                          -FileType "$installerType" `
                          -SilentArgs "$silentArgs" `
                          -Url64bit "$url64" `
                          -ValidExitCodes $validExitCodes `
                          -Checksum64 "$checksum64" `
                          -ChecksumType64 "$checksumType64"