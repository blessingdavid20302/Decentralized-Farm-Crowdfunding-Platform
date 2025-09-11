$content = Get-Content "contracts\FarmInsurancePool.clar" -Raw
$content = $content -replace "`r`n", "`n"
$content = $content -replace "`r", "`n"
[System.IO.File]::WriteAllText("contracts\FarmInsurancePool.clar", $content, [System.Text.UTF8Encoding]::new($false))
Write-Host "Line endings fixed!"
