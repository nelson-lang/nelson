$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.7.5/Nelson-0.7.5.2991-x86-32.exe'
$checksum = '7b06f78b77c5b7772c63ce4433127dab893b9778beebacaa6826ec49455e5e45'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.7.5/Nelson-0.7.5.2991-x86-64.exe'
$checksum64 = '509cec888e2fb5899c7c82cb5051a34a40503d857cecdbbf4dfe8075c01aef67'
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