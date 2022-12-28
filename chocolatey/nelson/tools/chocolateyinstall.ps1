$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.12/Nelson-0.6.12.2623-x86-32.exe'
$checksum = '18bb276da0e6022c0d8e580da84537c88b3c932f2d5ae16ba014780f216b3d53'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.12/Nelson-0.6.12.2623-x86-64.exe'
$checksum64 = '86d9bf04e94ac314883c9f430fab9f63f7ceb1c3d9e43c8ce05d20aed972d02b'
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