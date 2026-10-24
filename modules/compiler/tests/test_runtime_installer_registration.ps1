param(
    [Parameter(Mandatory)][string]$Installer,
    [Parameter(Mandatory)][ValidatePattern('^[0-9a-f]{64}$')][string]$Fingerprint,
    [ValidateSet('win64', 'woa64')][string]$Architecture = 'win64'
)
$ErrorActionPreference = 'Stop'
$root = Join-Path ([IO.Path]::GetTempPath()) ('nelson-runtime-registration-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $root | Out-Null
$key = "HKCU:\Software\Nelson\Runtime\$Architecture\$Fingerprint"
$uninstallKey = $null
if (Test-Path -LiteralPath $key) {
    throw 'An existing registration uses this fixture identity; do not modify it.'
}
$first = Join-Path $root 'registered'
$second = Join-Path $root 'portable'
function Invoke-Installer([string]$Path, [string]$Arguments) {
    $log = Join-Path $root ('operation-' + [Guid]::NewGuid().ToString('N') + '.log')
    # The uninstaller has two processes; -Wait includes the copied second phase.
    $process = Start-Process -FilePath $Path -ArgumentList ($Arguments + " /LOG=`"$log`"") `
        -Environment @{ TEMP = $root; TMP = $root } -WindowStyle Hidden -PassThru -Wait
    try {
        if ($process.ExitCode -ne 0) { Write-Host "INSTALLER_EXIT=$($process.ExitCode) LOG=$log" }
        return $process.ExitCode
    } finally { $process.Dispose() }
}
$Installer = (Resolve-Path -LiteralPath $Installer).Path
$arguments = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER'
try {
    $status = Invoke-Installer $Installer "$arguments /DIR=`"$first`""
    if ($status -ne 0 -or (Get-ItemPropertyValue -LiteralPath $key -Name Root) -ne $first) {
        throw 'Registered runtime installation failed.'
    }
    $entries = @(Get-ChildItem 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall' |
        Where-Object { $_.PSChildName.StartsWith("Nelson-Runtime-$Architecture-") } |
        Where-Object { (Get-ItemPropertyValue -LiteralPath $_.PSPath -Name InstallLocation -ErrorAction SilentlyContinue).TrimEnd('\') -eq $first })
    if ($entries.Count -ne 1) { throw 'Expected one registered runtime uninstaller.' }
    $uninstallKey = $entries[0].PSPath
    $status = Invoke-Installer $Installer "$arguments /DIR=`"$second`""
    if ($status -eq 0 -or (Test-Path -LiteralPath $second)) { throw 'Conflicting registration was not rejected before installation.' }
    $status = Invoke-Installer $Installer "$arguments /PORTABLE=1 /DIR=`"$second`""
    if ($status -ne 0) { throw 'Portable runtime installation failed.' }
    $uninstaller = @(Get-ChildItem -LiteralPath $second -Filter 'unins*.exe' -File)
    if ($uninstaller.Count -ne 1) { throw 'Expected one portable uninstaller.' }
    $status = Invoke-Installer $uninstaller[0].FullName '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART'
    if ($status -ne 0 -or (Get-ItemPropertyValue -LiteralPath $key -Name Root) -ne $first) {
        throw 'Portable uninstallation changed another runtime registration.'
    }
} finally {
    foreach ($destination in @($second, $first)) {
        if (-not [IO.Path]::GetFullPath($destination).StartsWith($root + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
            throw 'Uninstall destination escaped the test root.'
        }
        if (Test-Path -LiteralPath $destination) {
            foreach ($uninstaller in @(Get-ChildItem -LiteralPath $destination -Filter 'unins*.exe' -File)) {
                $status = Invoke-Installer $uninstaller.FullName '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART'
                if ($status -ne 0) { throw "Fixture uninstallation failed: $destination (exit $status)." }
            }
        }
    }
}
if ((Test-Path -LiteralPath $key) -or ($null -ne $uninstallKey -and (Test-Path -LiteralPath $uninstallKey))) {
    throw 'Runtime registration remained after uninstall.'
}
Write-Output "RUNTIME_REGISTRATION_PASS=$root"
