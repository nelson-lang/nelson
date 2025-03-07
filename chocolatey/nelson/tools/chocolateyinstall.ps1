$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.12.0/Nelson-1.12.0.4690-x86-64.exe'
$checksum64 = '6777ebd959ddd62ab91494e49fe3195934a8c98e218b386afb82609ce667b4fe'
$checksumType64 = 'sha256'
$validExitCodes = @(0)

Install-ChocolateyPackage -PackageName "$packageName" `
                          -FileType "$installerType" `
                          -SilentArgs "$silentArgs" `
                          -Url64bit "$url64" `
                          -ValidExitCodes $validExitCodes `
                          -Checksum64 "$checksum64" `
                          -ChecksumType64 "$checksumType64"