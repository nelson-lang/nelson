$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.5/Nelson-0.6.5.2005-x86-32.exe'
$checksum = '4f10ca379d4f156edd36941a0617e38afa6bd199fcc5d4b7322b86aad72caf4e'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.5/Nelson-0.6.5.2005-x86-64.exe'
$checksum64 = '29d3595da387596fe2006f4c3033bf8c61eee27574a9c68bf061120570766e5e'
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