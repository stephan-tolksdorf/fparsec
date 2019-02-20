# This PowerShell script builds the FParsec NuGet packages. 
#
# Run this script from the VS2017 Command Prompt, e.g. with 
# powershell -File pack.ps1 -versionSuffix "" > pack.out.txt

Param(
  [string]$versionSuffix = "dev"
)

$ErrorActionPreference = 'Stop'

$configs = $('Release-LowTrust', 'Release')

$testTargetFrameworks = @{'Release'          = $('net45')
                          'Release-LowTrust' = $('netcoreapp2.1', 'net45')}

function invoke([string] $cmd) {
    echo ''
    echo $cmd
    Invoke-Expression $cmd
    if ($LastExitCode -ne 0) {
        throw "Non-zero exit code: $LastExitCode"
    }
}

foreach ($folder in $("FParsecCS\obj", "FParsecCS\bin", "FParsec\obj", "FParsec\bin")) {
    try {
        Remove-Item $folder -recurse
    } catch {}
}

foreach ($config in $configs) {
    $props = "-c $config -p:VersionSuffix=$versionSuffix -p:FParsecNuGet=true"
    invoke "dotnet build FParsec $props"
    invoke "dotnet pack FParsec $props -o ""$pwd\bin\nupkg"""
    invoke "dotnet build Test $props"
    foreach ($tf in $testTargetFrameworks[$config]) {
        invoke "dotnet run --no-build -p Test -c $config -f $tf"
    }
}
