$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v1.6.0/Nelson-1.6.0.4208-x86-32.exe'
$checksum = '617c11985f07ad9d774283a2659e3e394e010b979fd41449df83308ad3b857b4'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.6.0/Nelson-1.6.0.4208-x86-64.exe'
$checksum64 = '4e9b483a43a7685ccb584067c62dbbd7111c097b800dcf158d8c56dcd693db71'
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