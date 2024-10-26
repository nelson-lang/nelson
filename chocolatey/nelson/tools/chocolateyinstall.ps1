$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v1.9.0/Nelson-1.9.0.4392-x86-32.exe'
$checksum = '8e7def877b63c65f644358ed8210f3f3aaff76c7022b05b5bd39ab7fcc54ab18'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.9.0/Nelson-1.9.0.4392-x86-64.exe'
$checksum64 = '0466a3d184938e9a4ffb4fc2f7387735acdd4bf8b3c1a5c0e94535842ee5968d'
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