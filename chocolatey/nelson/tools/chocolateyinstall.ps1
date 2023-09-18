$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.7.9/Nelson-0.7.9.3259-x86-32.exe'
$checksum = '2fa9968b9dd4f883142cab77d5c55980195b531e2cf3c8b606b614f446c59d38'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.7.9/Nelson-0.7.9.3259-x86-64.exe'
$checksum64 = 'c319f38624c9aa8cde00b5366291e6381352da4aa39720f6b261af84fc0788eb'
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