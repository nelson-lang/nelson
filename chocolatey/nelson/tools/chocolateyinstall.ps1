$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v1.4.0/Nelson-1.4.0.4010-x86-32.exe'
$checksum = '7ee3c3af45d64990bc60aecd2c6fe0526f1971d2ceb0841cd6a6c6c36d7a88cc'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.4.0/Nelson-1.4.0.4010-x86-64.exe'
$checksum64 = '888a5d7b1ebfacc413c665f07d2c40d82b30f1dd18056cf00d962d0a210ecb35'
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