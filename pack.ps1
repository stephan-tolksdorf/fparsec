# This PowerShell script builds the FParsec NuGet packages. 
# Currently, it uses msbuild and needs to be run in the VS2017 Command Prompt, 
# so that it can build PCL assemblies. 
# It also requires the nuget.exe, see $nuget below.
#
# Run this script from the VS2017 Command Prompt, e.g. with 
# powershell -File pack.ps1 -versionSuffix "" > pack.out.txt

Param(
  [string]$versionSuffix = "dev",
  [string]$nuget = ".\nuget.exe"
)

$ErrorActionPreference = 'Stop'

$configs = $('Release-LowTrust', 'Release')

$testTargetFrameworks = @{'Release'          = $('net45', 'net40-client')
                          'Release-LowTrust' = $('netcoreapp1.1', 'net45')}
$testPCL = $true

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
    $props = "/p:Configuration=$config /p:VersionSuffix=$versionSuffix /p:FParsecNuGet=true"
    invoke "msbuild /t:Restore $props"
    invoke "msbuild /t:Clean $props"
    invoke "msbuild FParsec /t:Build $props"
    invoke "msbuild Test /t:Build $props"
    foreach ($tf in $testTargetFrameworks[$config]) {
        if ($tf -eq 'netcoreapp1.1') {
            invoke "dotnet .\Test\bin\$config\$tf\Test.dll"
        } else {
            invoke ".\Test\bin\$config\$tf\Test.exe"
        }
    }
    invoke "msbuild /t:Restore $props /p:MergedFParsecPackage=true"
    invoke "msbuild /t:Pack /p:NoBuild=true /p:IncludeSource=true /p:IncludeSymbols=true $props /p:MergedFParsecPackage=true"
    if (($config -eq 'Release-LowTrust') -and $testPCL) {
        $pclProps = "/p:TargetFramework=net45 /p:OutputPath=bin\$config\net45-pcl\ /p:TestPCLFParsec=true $props"
        invoke "msbuild Test/Test.fsproj /t:Clean $pclProps"
        invoke "msbuild Test/Test.fsproj /t:Build $pclProps"
        invoke ".\Test\bin\$config\net45-pcl\Test.exe"
    }
}

# The non-symbol packages built by the msbuild Pack target include some files only belonging
# into the symbol packages, so we have to recreate the packages from the generated nuspecs.
foreach ($nuspec in Get-ChildItem -Path ".\FParsec\obj\FParsec*.symbols.nuspec") {
    $localNuspecPath = ".\$($nuspec.Name.Replace('.symbols', [string]::Empty))"
    Copy-Item $nuspec $localNuspecPath -verbose
    invoke ".\nuget pack $localNuspecPath -Symbols"
}
