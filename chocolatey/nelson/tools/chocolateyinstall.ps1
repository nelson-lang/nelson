$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v0.7.12/Nelson-0.7.12.3590-x86-32.exe'
$checksum = 'f920cedba1aa85ecdc850cbd8c42959d2ac468da122090e8594d3b437f5bb6c1'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v0.7.12/Nelson-0.7.12.3590-x86-64.exe'
$checksum64 = 'ef10f414c3a35b9d5b748d0840293bab73505e793ae33621f11f21670fae2bc3'
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