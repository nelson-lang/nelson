$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v1.3.0/Nelson-1.3.0.3872-x86-32.exe'
$checksum = '8709dfcf245c0aa4b5e1201610c49eea55e81bb763b7257cc4d0f0f4d7169c43'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.3.0/Nelson-1.3.0.3872-x86-64.exe'
$checksum64 = '18891898d5da77f83c622358ab5919c39fb8c9cb7d9fb7170de27295a7e472a4'
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