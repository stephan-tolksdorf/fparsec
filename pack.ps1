# This PowerShell script builds the FParsec NuGet packages. 
#
# Run this script from the VS2019 Command Prompt, e.g. with 
#     powershell -ExecutionPolicy ByPass -File pack.ps1 -versionSuffix "" > pack.out.txt
# or on macOS e.g. with
#     pwsh -File pack.ps1 -versionSuffix "" > pack.out.txt

Param(
  [string]$versionSuffix = "dev"
)

$ErrorActionPreference = 'Stop'

$configSuffices = $('-LowTrust') # The non-LowTrust version currently doesn't pass the tests.

$testTargetFrameworks = @{''          = $('net6')
                          '-LowTrust' = $('net6')}

function invoke([string] $cmd) {
    echo ''
    echo $cmd
    Invoke-Expression $cmd
    if ($LastExitCode -ne 0) {
        throw "Non-zero exit code: $LastExitCode"
    }
}

foreach ($folder in $("nupkgs", "FParsecCS\obj", "FParsecCS\bin", "FParsec\obj", "FParsec\bin")) {
    try {
        Remove-Item $folder -recurse
    } catch {}
}

foreach ($configSuffix in $configSuffices) {
    $config = "Release$configSuffix"
    $props = "-c $config -p:VersionSuffix=$versionSuffix -p:FParsecNuGet=true -p:Platform='Any CPU'"
    invoke "dotnet build FParsec/FParsec$configSuffix.fsproj $props -v n"
    invoke "dotnet pack FParsec/FParsec$configSuffix.fsproj $props -o ""$pwd\nupkgs"""
    invoke "dotnet build Test/Test$configSuffix.fsproj $props -v n"
    foreach ($tf in $testTargetFrameworks[$configSuffix]) {
        # invoke "dotnet run --no-build --project Test/Test$configSuffix.fsproj $props -f $tf" # doesn't work properly
        invoke "Test/bin/'Any CPU'/$config/$tf/Test"
    }
}
