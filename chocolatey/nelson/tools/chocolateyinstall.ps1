$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.7.4/Nelson-0.7.4.2957-x86-32.exe'
$checksum = 'e8a9bb9bbf64096d4d045a7ee64a3b28a33b9ddb5079cc2603f13e0a988955b9'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.7.4/Nelson-0.7.4.2957-x86-64.exe'
$checksum64 = '796515560d9fcdf6e905d7a1ae8dc4105684f88bebe9c97e2239182a4c4fd32a'
$checksumType64 = 'sha256'
$validExitCodes = @(0)

Install-ChocolateyPackage -PackageName "$packageName" `
                          -FileType "$installerType" `
                          -SilentArgs "$silentArgs" `
                          -Url "$url" `
                          -Url64bit "$url64" `
                          -ValidExitCodes $validExitCodes `
                          -Checksum "$checksum" `
                          -ChecksumType "$checksumType" `
                          -Checksum64 "$checksum64" `
                          -ChecksumType64 "$checksumType64"