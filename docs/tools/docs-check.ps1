param(
  [string]$Root = "../docs"
)

Write-Host "Running documentation checks under $Root" -ForegroundColor Cyan

# 1) 标题双语与编号样式检查（粗略正则）
Get-ChildItem -Recurse "$Root" -Filter README.md | ForEach-Object {
  $p = $_.FullName
  $content = Get-Content $p -Raw
  if ($content -notmatch "##\s+\d+\.\s+.+\s+/\s+.+") {
    Write-Host "[Title/Bilingual Warning] $p" -ForegroundColor Yellow
  }
}

# 2) 内部相对链接与锚点基本检查（仅检查目标文件存在性）
Get-ChildItem -Recurse "$Root" -Filter README.md | ForEach-Object {
  $p = $_.FullName
  $lines = Get-Content $p
  $i = 0
  foreach ($line in $lines) {
    $i++
    if ($line -match "\]\(\.\./.+README.md#.+\)") {
      $m = [regex]::Match($line, "\]\((\S+README.md)#")
      if ($m.Success) {
        $target = Join-Path (Split-Path $p) $m.Groups[1].Value
        if (-not (Test-Path $target)) {
          Write-Host "[Broken Link] $p:$i -> $($m.Groups[1].Value)" -ForegroundColor Red
        }
      }
    }
  }
}

# 3) 外链包含DOI或稳定URL的提示（仅提示）
Get-ChildItem -Recurse "$Root" -Filter README.md | ForEach-Object {
  $p = $_.FullName
  $content = Get-Content $p -Raw
  if ($content -match "\]\(https?://" -and $content -notmatch "doi\.org") {
    Write-Host "[External Link Notice] Consider adding DOI for: $p" -ForegroundColor Yellow
  }
}

Write-Host "Checks finished." -ForegroundColor Green
