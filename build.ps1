$version = '0.7.0' # the version under development, update after a release
$versionSuffix = '-build.0' # manually incremented for local builds

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse($tag, [ref]$v)
}

if ($env:appveyor){
    $versionSuffix = '-build.' + $env:appveyor_build_number
    if ($env:appveyor_repo_tag -eq 'true' -and (isVersionTag($env:appveyor_repo_tag_name))){
        $version = $env:appveyor_repo_tag_name
        $versionSuffix = ''
    }
    Update-AppveyorBuild -Version "$version$versionSuffix"
}

dotnet build -c Release Froto.sln /p:Version=$version$versionSuffix
dotnet test --no-build -c Release Parser.Test\Froto.Parser.Test.fsproj
dotnet test --no-build -c Release Serialization.Test\Froto.Serialization.Test.fsproj
dotnet pack --no-build -c Release Parser\Froto.Parser.fsproj /p:Version=$version$versionSuffix -o $psscriptroot/bin
dotnet pack --no-build -c Release Serialization\Froto.Serialization.fsproj /p:Version=$version$versionSuffix -o $psscriptroot/bin