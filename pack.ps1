# This PowerShell script builds the FParsec NuGet packages. 
#
# Run this script from the VS2019 Command Prompt, e.g. with 
# powershell -ExecutionPolicy ByPass -File pack.ps1 -versionSuffix "" > pack.out.txt

Param(
  [string]$versionSuffix = "dev"
)

$ErrorActionPreference = 'Stop'

$configs = $('Release-LowTrust', 'Release')

$testTargetFrameworks = @{'Release'          = $('net45')
                          'Release-LowTrust' = $('netcoreapp3.0', 'net45')}

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

foreach ($config in $configs) {
    $props = "-c $config -p:VersionSuffix=$versionSuffix -p:FParsecNuGet=true -p:Platform='Any CPU'"
    invoke "dotnet build FParsec/FParsec.fsproj $props -v n"
    invoke "dotnet pack FParsec/FParsec.fsproj $props -o ""$pwd\nupkgs"""
    invoke "dotnet build Test/Test.fsproj $props -v n"
    foreach ($tf in $testTargetFrameworks[$config]) {
        invoke "dotnet run --no-build -p Test/Test.fsproj -c $config -f $tf"
    }
}
