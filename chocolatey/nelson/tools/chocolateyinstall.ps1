$packageName = 'nelson'
$installerType = 'exe'
$silentArgs = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-'
$url = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.8/Nelson-0.6.9.2345-x86-32.exe'
$checksum = 'd00a56e5dafdd49c745092284d16f4b2cad9a4e0c8fec354934d8abe44ff383a'
$checksumType = 'sha256'
$url64 = 'https://github.com/Nelson-numerical-software/nelson/releases/download/v0.6.8/Nelson-0.6.9.2345-x86-64.exe'
$checksum64 = '455eea63ecc49ff6c8fc2369a9292d019e913a038a255bcff9ee701882d8a516'
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