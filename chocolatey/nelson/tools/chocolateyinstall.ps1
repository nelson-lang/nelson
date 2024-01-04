$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /ALLUSERS /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/nelson-lang/nelson/releases/download/v1.0.0/Nelson-1.0.0.3620-x86-32.exe'
$checksum = '95b358a9634e4c241fe492103e826bbba92cd22f5423c863247c04dca4bb5ab8'
$checksumType = 'sha256'
$url64 = 'https://github.com/nelson-lang/nelson/releases/download/v1.0.0/Nelson-1.0.0.3620-x86-64.exe'
$checksum64 = '8b1dbd3ea023082a8ce8ce518db115ca108dfdd858e27d955260d60a60623829'
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