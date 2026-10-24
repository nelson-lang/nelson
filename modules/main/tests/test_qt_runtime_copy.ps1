#Requires -Version 7.0
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/qt-runtime-copy-' + [Guid]::NewGuid().ToString('N'))
$sdk = Join-Path $root 'fake SDK'
$sdkBin = Join-Path $sdk 'bin'
$destination = Join-Path $root 'runtime binaries'
New-Item -ItemType Directory -Path $sdkBin,$destination | Out-Null
$helper = Join-Path $repo 'tools/install_dependencies/copy-qt-runtime.bat'
$names = @('Qt6Core.dll','Qt6Cored.dll','Qt6LabsStyleKitImpl.dll','Qt6LabsStyleKitImpld.dll','Qt6FutureModule.dll')
foreach ($name in $names + @('unrelated.dll')) {
    [IO.File]::WriteAllBytes((Join-Path $sdkBin $name), [Text.Encoding]::ASCII.GetBytes($name))
}
[IO.File]::WriteAllBytes((Join-Path $destination 'nelson-owned.dll'), [byte[]]@(1,2,3))
function Invoke-Copy([string]$Source, [string]$Destination) {
    $info = [Diagnostics.ProcessStartInfo]::new($env:ComSpec)
    $info.Arguments = '/d /s /c ""' + $helper + '" "' + $Source + '" "' + $Destination + '""'
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    $process = [Diagnostics.Process]::Start($info)
    $out = $process.StandardOutput.ReadToEndAsync()
    $err = $process.StandardError.ReadToEndAsync()
    if (-not $process.WaitForExit(15000)) {
        $process.Kill($true)
        $process.WaitForExit()
        throw 'Qt runtime copy timed out.'
    }
    $result = [pscustomobject]@{ ExitCode=$process.ExitCode; Output=$out.Result; Error=$err.Result }
    $process.Dispose()
    return $result
}
$run = Invoke-Copy $sdk $destination
if ($run.ExitCode -ne 0) { throw ($run.Output + $run.Error) }
foreach ($name in $names) {
    if ((Get-FileHash -LiteralPath (Join-Path $sdkBin $name)).Hash -ne (Get-FileHash -LiteralPath (Join-Path $destination $name)).Hash) {
        throw "Qt copy differs: $name"
    }
}
if (Test-Path -LiteralPath (Join-Path $destination 'unrelated.dll')) { throw 'Non-Qt library copied.' }
if ([Convert]::ToBase64String([IO.File]::ReadAllBytes((Join-Path $destination 'nelson-owned.dll'))) -ne 'AQID') {
    throw 'Unrelated destination file changed.'
}
$target = Join-Path $destination 'Qt6Core.dll'
$source = Join-Path $sdkBin 'Qt6Core.dll'
$timestamp = [IO.File]::GetLastWriteTimeUtc($source)
[IO.File]::WriteAllBytes($target, [byte[]]::new((Get-Item -LiteralPath $source).Length))
[IO.File]::SetLastWriteTimeUtc($target, $timestamp)
$run = Invoke-Copy $sdk $destination
if ($run.ExitCode -ne 0 -or (Get-FileHash -LiteralPath $source).Hash -ne (Get-FileHash -LiteralPath $target).Hash) {
    throw 'Same-size/time Qt destination was not refreshed.'
}
$lock = [IO.File]::Open($target, [IO.FileMode]::Open, [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
try {
    $run = Invoke-Copy $sdk $destination
    if ($run.ExitCode -eq 0) { throw 'Locked Qt library copy was accepted.' }
} finally { $lock.Dispose() }
$missingDestination = Join-Path $root 'missing runtime'
$run = Invoke-Copy (Join-Path $root 'missing SDK') $missingDestination
if ($run.ExitCode -eq 0 -or (Test-Path -LiteralPath $missingDestination)) { throw 'Missing SDK was not rejected before copying.' }
'PASS complete Qt SDK inventory, release/debug, paths with spaces, replacement, unrelated files and copy errors'
"QT_COPY_TEST=$root"
