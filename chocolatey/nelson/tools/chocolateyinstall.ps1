$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v1.5.0/Nelson-1.5.0.4088-x86-32.exe'
$checksum = 'e24a26e825a791e353fb0821962f13ccf77bbee62b7e5d74d9e9a4bf3d4129e9'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.5.0/Nelson-1.5.0.4088-x86-64.exe'
$checksum64 = '15bcacf4612b6d40c2a1cded8b9278cab211aa0aad00f1d796152031aaef0f30'
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