$ErrorActionPreference = "Stop"

function checkExitCode {
    if ($LastExitCode -ne 0) {
        throw "Non-zero exit code: $LastExitCode"
    }
}

$projs = $("FParsecCS"; "FParsec")

dotnet clean
dotnet restore
checkExitCode

dotnet build -c Release --version-suffix "dev"
checkExitCode

# Samples

dotnet .\Samples\Calculator\bin\Release\netcoreapp1.1\Calculator.dll
checkExitCode

dotnet .\Samples\FSharpParsingSample\FParsecVersion\bin\Release\netcoreapp1.1\InterpFParsec.dll .\Samples\FSharpParsingSample\LexYaccVersion\test.lang
checkExitCode

dotnet .\Samples\JSON\bin\Release\netcoreapp1.1\JsonParser.dll .\Samples\JSON\test_json.txt
checkExitCode

dotnet .\Samples\PEG\bin\Release\netcoreapp1.1\PegParser.dll .\Samples\PEG\test_peg.txt
checkExitCode

dotnet .\Samples\Tutorial\bin\Release\netcoreapp1.1\Tutorial.dll
checkExitCode

.\Samples\Calculator\bin\Release\net45\Calculator.exe
checkExitCode

.\Samples\FSharpParsingSample\FParsecVersion\bin\Release\net45\InterpFParsec.exe .\Samples\FSharpParsingSample\LexYaccVersion\test.lang
checkExitCode

.\Samples\JSON\bin\Release\net45\JsonParser.exe .\Samples\JSON\test_json.txt
checkExitCode

.\Samples\PEG\bin\Release\net45\PegParser.exe .\Samples\PEG\test_peg.txt
checkExitCode

.\Samples\Tutorial\bin\Release\net45\Tutorial.exe
checkExitCode

# Tests

dotnet .\Test\bin\Release\netcoreapp1.1\Test.dll
checkExitCode

.\Test\bin\Release\net45\Test.exe
checkExitCode

dotnet pack --include-symbols --include-source -c Release --version-suffix "dev"
