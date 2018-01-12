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

$version = '1.0.4'
$packageVersion = $version + '-RC'
if ($env:appveyor){ $packageVersion = packageVersion + 'pdb' + [int]::Parse($env:appveyor_build_number).ToString('000') }

$configs = $('Release-LowTrust', 'Release')

$testTargetFrameworks = @{'Release'          = $('net45', 'net40-client')
                          'Release-LowTrust' = $('netcoreapp2.0', 'net45')}
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
    invoke "msbuild FParsec /t:Build /p:SourceLinkCreate=true $props /p:Version=$version"
    invoke "msbuild Test /t:Build $props"
    foreach ($tf in $testTargetFrameworks[$config]) {
        if ($tf.StartsWith('netcoreapp')) {
            invoke "dotnet .\Test\bin\$config\$tf\Test.dll"
        } else {
            invoke ".\Test\bin\$config\$tf\Test.exe"
        }
    }
    invoke "msbuild /t:Restore $props /p:MergedFParsecPackage=true"
    invoke "msbuild /t:Pack /p:NoBuild=true $props /p:MergedFParsecPackage=true /p:PackageVersion=$packageVersionu"
    if (($config -eq 'Release-LowTrust') -and $testPCL) {
        $pclProps = "/p:TargetFramework=net45 /p:OutputPath=bin\$config\net45-pcl\ /p:TestPCLFParsec=true $props"
        invoke "msbuild Test/Test.fsproj /t:Clean $pclProps"
        invoke "msbuild Test/Test.fsproj /t:Build $pclProps"
        invoke ".\Test\bin\$config\net45-pcl\Test.exe"
    }
}