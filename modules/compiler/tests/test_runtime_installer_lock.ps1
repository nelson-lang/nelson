param(
    [Parameter(Mandatory)][string]$Installer,
    [Parameter(Mandatory)][ValidatePattern('^[0-9a-f]{64}$')][string]$Fingerprint,
    [ValidateSet('win64', 'woa64')][string]$Architecture = 'win64',
    [switch]$Uninstall
)
$ErrorActionPreference = 'Stop'
$root = Join-Path ([IO.Path]::GetTempPath()) ('nelson-runtime-lock-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $root | Out-Null
$log = Join-Path $root 'install.log'
$destination = Join-Path $root 'runtime'
$created = $false
$mutex = [Threading.Mutex]::new($false, "Global\Nelson-Runtime-$Architecture-$Fingerprint-operation", [ref]$created)
if (-not $created) { $mutex.Dispose(); throw 'The test runtime operation lock already exists.' }
$Installer = (Resolve-Path -LiteralPath $Installer).Path
$arguments = "/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /LOG=`"$log`""
if (-not $Uninstall) { $arguments += " /SP- /CURRENTUSER /PORTABLE=1 /DIR=`"$destination`"" }
$process = $null
try {
    $process = Start-Process -FilePath $Installer -ArgumentList $arguments -WindowStyle Hidden `
        -Environment @{ TEMP = $root; TMP = $root } -PassThru -Wait
    if ($process.ExitCode -eq 0) { throw 'Installer ignored an existing runtime operation lock.' }
    $content = Get-Content -LiteralPath $log -Raw
    if ($content -notmatch 'Another operation is using this shared runtime') { throw 'Installer did not reject the runtime lock.' }
    if (Test-Path -LiteralPath $destination) { throw 'Failed preflight changed the runtime destination.' }
    if (-not (Test-Path -LiteralPath $Installer)) { throw 'Blocked operation removed its own executable.' }
    Write-Output "RUNTIME_LOCK_PASS=$root"
} finally {
    if ($null -ne $process) { $process.Dispose() }
    $mutex.Dispose()
}
