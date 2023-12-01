$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v0.7.11/Nelson-0.7.11.3470-x86-32.exe'
$checksum = '84a941bade19c42a6492e240b4d03107c633a2da1efe84a7ce14baf261ca7129'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v0.7.11/Nelson-0.7.11.3470-x86-64.exe'
$checksum64 = '916ccfb2169c296f4e1632c9f91d8323b31078351deb45e1cced890385976bbc'
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