Import-Module ActiveDirectory

function Get-UserInfo {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Username
    )
    
    $user = Get-ADUser -Identity $Username
    return $user
}

function Set-UserConfig {
    param(
        [string]$Key,
        [string]$Value
    )
    
    $config = @{$Key = $Value}
    return $config
}

function Remove-TempFiles {
    param([string]$Path = $env:TEMP)
    
    Get-ChildItem -Path $Path -Filter "*.tmp" | Remove-Item
}
