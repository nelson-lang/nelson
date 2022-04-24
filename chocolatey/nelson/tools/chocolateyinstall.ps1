$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.4/Nelson-0.6.4.1909-x86-32.exe'
$checksum = 'add2e7e79c867eeea9963569f950fadce18802717047a27b6b716fa1e4da0b67'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.4/Nelson-0.6.4.1909-x86-64.exe'
$checksum64 = '06cc4f162cb4d3d3cd40f18458137a54b6a8b3824f77a8528327fc8f33d9f868'
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