// This script builds the NuGet packages. Run it with `fsi build.fsx`.
//
// If the nuget.exe is not located in the same directory as this script, you'll
// need change the nugetExe constant below.
//
// If the script directory also contains a public/private signing key named
// "PublicPrivateKey.snk" the assemblies will get signed with it.

open System.Diagnostics
open System.Reflection

let normalizePath = System.IO.Path.GetFullPath

let thisDir = __SOURCE_DIRECTORY__

let nugetExe = normalizePath (thisDir + "/nuget.exe")

let keyFilePath = normalizePath (thisDir + "/PublicPrivateKey.snk")

let nameDefault = "FParsec"
let nameBigData = "FParsec-Big-Data-Edition"

type Framework = Net40Client | Net45

type Config = {Framework: Framework
               Defines: string
               UsesUnsafeCode: bool}

type Package = {Id: string
                Title: string
                Version: string
                Authors: string
                Owners: string
                LicenseUrl: string
                ProjectUrl: string
                Description: string
                ReleaseNotes: string
                Copyright: string
                Tags: string
                Configs: Config list}

let version =
    let str = System.IO.File.ReadAllText(thisDir + "/../../FParsecCS/Properties/AssemblyInfo.cs")
    let mark = "public const string FileVersion = \""
    let i0 = str.IndexOf(mark) + mark.Length
    let i1 = str.IndexOf(".0\"", i0) - 1
    if i1 < 0 || i0 >= i1 then failwith "Could not determine assembly version"
    str.[i0..i1] // + "-RC1"

let authors = "Stephan Tolksdorf"
let copyright = "Copyright © 2013 Stephan Tolksdorf"
let tags = "parser, combinator, f#, fsharp, c#, csharp"

let commonDescription = """FParsec is a parser combinator library for F#.

You can find comprehensive documentation for FParsec at http://www.quanttec.com/fparsec. The documentation includes a feature list, a tutorial, a user’s guide and an API reference."""

let lowTrustDescription = """

This package uses the basic “low-trust” configuration of FParsec, which does not use any unverifiable code and is optimized for maximum portability. If you need to parse very large files or if you employ FParsec for performance-critical jobs, consider using the alternate “Big Data Edition” NuGet package (see nuget.org/packages/fparsec-big-data-edition).
"""

let bigdataDescription = """

This package uses a configuration of FParsec that supports very large input streams and is optimized for maximum performance in longer running processes. See www.quanttec.com/fparsec/download-and-installation.html for more information.
"""

let packages =
   [{Id = "FParsec"
     Title = "FParsec"
     Version = version
     Authors = authors
     Owners = authors
     LicenseUrl = "http://www.quanttec.com/fparsec/license.html"
     ProjectUrl = "http://www.quanttec.com/fparsec/"
     Description = commonDescription + lowTrustDescription
     ReleaseNotes = "See http://www.quanttec.com/fparsec/about/changelog.html"
     Copyright = copyright
     Tags = tags
     Configs = [{Framework = Net40Client
                 Defines = "CLR4;LOW_TRUST;SMALL_STATETAG"
                 UsesUnsafeCode = false}]};

   {Id = "FParsec-Big-Data-Edition"
    Title = "FParsec (Big Data Edition)"
    Version = version
    Authors = authors
    Owners = authors
    LicenseUrl = "http://www.quanttec.com/fparsec/license.html"
    ProjectUrl = "http://www.quanttec.com/fparsec/"
    Description = commonDescription + bigdataDescription
    ReleaseNotes = "See http://www.quanttec.com/fparsec/about/changelog.html"
    Copyright = copyright
    Tags = tags
    Configs = [{Framework = Net40Client
                Defines = "CLR4;UNALIGNED_READS;USE_STATIC_MAPPING_FOR_IS_ANY_OF"
                UsesUnsafeCode = true}
               {Framework = Net45
                Defines = "CLR4;CLR45;UNALIGNED_READS;USE_STATIC_MAPPING_FOR_IS_ANY_OF"
                UsesUnsafeCode = true}]
   }]

let msBuildPath =
    (Microsoft.Win32.Registry.GetValue(
        "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\MSBuild\ToolsVersions\4.0",
        "MSBuildToolsPath",
        "") :?> string) + "\MsBuild.exe"

let getOutputPath packageId framework =
    let libName = match framework with
                  | Net40Client -> "net40-client"
                  | Net45 -> "net45"
    normalizePath (thisDir + "/" + packageId + "/lib/" + libName)

let keyExists = System.IO.File.Exists(keyFilePath)

let publicKeyString =
    if not keyExists then null
    else
        StrongNameKeyPair(System.IO.File.ReadAllBytes(keyFilePath)).PublicKey
        |> Array.map (fun b -> sprintf "%02x" b)
        |> String.concat ""

let getMsBuildArgs outputPath {Framework = framework
                               Defines = defines
                               UsesUnsafeCode = unsafe} =

    let targetFrameworkVersion, targetFrameworkProfile =
        match framework with
        | Net40Client -> "v4.0", "Client"
        | Net45 -> "v4.5", ""

    let ps = ["Configuration", "nuget"
              "OutputPath", outputPath
              "TargetFrameworkVersion", targetFrameworkVersion
              "TargetFrameworkProfile", targetFrameworkProfile
              "DefineConstants", defines + ";NUGET"
              "AllowUnsafeBlocks", (if unsafe then "true" else "false")
              "Optimize", "true"
              "TailCalls", "false"
              "DebugSymbols", "true"
              "DebugType", "pdbonly"
              "OtherFlags", "/nooptimizationdata"
              "WarningLevel", "4"
              "NoWarn", "1591"
              "ErrorReport", "prompt"]
             |> Seq.append
                    (if keyExists then
                         ["SignAssembly", "true"
                          "AssemblyOriginatorKeyFile", keyFilePath]
                     else [])
             |> Seq.map (fun (k, v) -> "/p:" + k + "=\"" + v + "\"")
             |> String.concat " "
    let path = normalizePath (thisDir + "/../VS11/Test.fsproj")
    "\"" + path + "\" /t:rebuild " + ps

let writeExtraAssemblyInfo dir (configInfo: string) =
    System.IO.Directory.CreateDirectory(dir) |> ignore

    let configInfo = configInfo.Replace("\\", "\\\\")
                               .Replace("\"", "\\\"")

    let key = if keyExists then ", PublicKey=" + publicKeyString else ""

    let test =
        System.String.Format("""
namespace FParsec {{
internal static partial class CommonAssemblyInfo {{
    public const string Configuration = "NuGet build ({0})";
    public const string FParsecAssemblyName = "FParsec{1}";
    public const string TestAssemblyName = "test_fparsec{1}";
}};
}}
""", configInfo, key)

    System.IO.File.WriteAllText(dir + "/ExtraAssemblyInfo.cs",
                                test,
                                System.Text.Encoding.UTF8)

let deleteExtraAssemblyInfo dir =
    System.IO.File.Delete(dir + "/ExtraAssemblyInfo.cs")


let writeNugetSpec (package: Package) =
    let path = normalizePath (thisDir + "/" + package.Id + "/"
                              + package.Id + ".nuspec")

    let ps = ["id", package.Id
              "title",  package.Title
              "version", package.Version
              "authors", package.Authors
              "owners", package.Owners
              "licenseUrl", package.LicenseUrl
              "projectUrl", package.ProjectUrl
              "description", package.Description
              "releaseNotes", package.ReleaseNotes
              "copyright", package.Copyright
              "tags", package.Tags]
    let text =
        "<?xml version=\"1.0\"?>\n<package>\n  <metadata>\n"
        + (ps
           |> Seq.map (fun (k, v) ->
                 sprintf "    <%s>%s</%s>"
                         k (System.Security.SecurityElement.Escape(v)) k
              )
           |> String.concat System.Environment.NewLine)
        + "\n  </metadata>\n</package>"

    System.IO.File.WriteAllText(path, text, System.Text.Encoding.UTF8)

let rec copyDir srcDir dstDir =
    let srcDirInfo = new System.IO.DirectoryInfo(srcDir)
    if not (System.IO.Directory.Exists(dstDir)) then
        System.IO.Directory.CreateDirectory(dstDir) |> ignore
    for dir in srcDirInfo.GetDirectories() do
        copyDir dir.FullName (System.IO.Path.Combine(dstDir, dir.Name))
    for file in srcDirInfo.GetFiles() do
        file.CopyTo(System.IO.Path.Combine(dstDir, file.Name), true) |> ignore

let copySources packageId =
    // This is the lazy approach. It would be better to read the project file
    // and only copy the referenced source files.
    let packageSrcDir = thisDir + "/" + packageId + "/src"
    if System.IO.Directory.Exists(packageSrcDir) then
        System.IO.Directory.Delete(packageSrcDir, true)
    copyDir (thisDir + "/../../FParsecCS") (packageSrcDir + "/FParsecCS")
    copyDir (thisDir + "/../../FParsec") (packageSrcDir + "/FParsec")

let exec program args =
    let startInfo = new ProcessStartInfo()
    startInfo.FileName <- program
    startInfo.Arguments <- args
    startInfo.UseShellExecute <- false

    let proc = Process.Start(startInfo)
    proc.WaitForExit()
    let exitCode = proc.ExitCode
    if exitCode <> 0 then
      System.Environment.Exit(exitCode)

printfn "Building NuGet packages..."

for package in packages do
    for config in package.Configs do
        let outputPath = getOutputPath package.Id config.Framework
        let args = getMsBuildArgs outputPath config
        let configInfo = args.Substring(args.IndexOf("/p:"))
                             .Replace(outputPath, "(...)")
                             .Replace(thisDir, "(...)")
        writeExtraAssemblyInfo outputPath configInfo
        printfn "%s" (msBuildPath + " " + args)
        exec msBuildPath args
        deleteExtraAssemblyInfo outputPath

        let testExe = outputPath + "/test_fparsec.exe"
        printfn "%s" testExe
        exec testExe ""
        System.IO.File.Delete(testExe)
        System.IO.File.Delete( outputPath + "/test_fparsec.pdb")
    copySources package.Id
    writeNugetSpec package
    let nugetArgs = "pack " + package.Id + "/" + package.Id + ".nuspec -Symbols"
                            + " -OutputDirectory " + package.Id + "/"
    printfn "%s %s" nugetExe nugetArgs
    exec nugetExe nugetArgs

if keyExists then
    printfn "\nThe assemblies were signed with %s." keyFilePath
else
    printfn "\nThe assemblies were NOT signed."

printfn "\nYou should double-check that the PackageId/src subdirectories don't \
        contain any files you don't want to be published as part of the symbol \
        package!"
