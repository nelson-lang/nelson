$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.15.0/Nelson-1.15.0.5475-x86-64.exe'
$checksum64 = 'sha256:eacd4a9109094678c43a4c9de91d185b1357ef8ed0e8f49c4ac7849c0b608e99'
$checksumType64 = 'sha256'
$validExitCodes = @(0, 1638, 3010)

Install-ChocolateyPackage -PackageName "$packageName" `
                          -FileType "$installerType" `
                          -SilentArgs "$silentArgs" `
                          -Url64bit "$url64" `
                          -ValidExitCodes $validExitCodes `
                          -Checksum64 "$checksum64" `
                          -ChecksumType64 "$checksumType64"