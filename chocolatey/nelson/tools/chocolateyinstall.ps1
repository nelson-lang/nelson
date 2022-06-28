$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.6/Nelson-0.6.6.2086-x86-32.exe'
$checksum = '05f76bf40fa963ed1019b918bfc3445c559bec2a2cb652a603e3f7318c36a9b7'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.6/Nelson-0.6.6.2086-x86-64.exe'
$checksum64 = '07ac4ff636a13618265996c456cb58b35bd909a16647e3286222425c6eb9bd83'
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