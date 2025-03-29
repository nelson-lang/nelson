$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.13.0/Nelson-1.13.0.4854-x86-64.exe'
$checksum64 = '428d2e36810472f4717730ef53456df8d8072aa027aea5aa5d5db1f868b69e39'
$checksumType64 = 'sha256'
$validExitCodes = @(0)

Install-ChocolateyPackage -PackageName "$packageName" `
                          -FileType "$installerType" `
                          -SilentArgs "$silentArgs" `
                          -Url64bit "$url64" `
                          -ValidExitCodes $validExitCodes `
                          -Checksum64 "$checksum64" `
                          -ChecksumType64 "$checksumType64"