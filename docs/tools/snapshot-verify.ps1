param(
  [string]$SnapshotDir = "../../data/snapshots"
)

Write-Host "Verifying snapshots under $SnapshotDir" -ForegroundColor Cyan
if (-not (Test-Path $SnapshotDir)) { throw "Snapshot directory not found: $SnapshotDir" }

Get-ChildItem $SnapshotDir -Filter *.sha256 | ForEach-Object {
  $p = $_.FullName
  $lines = Get-Content $p | Where-Object { $_ -match "\S" }
  foreach ($line in $lines) {
    $parts = $line -split "\s\s+"
    if ($parts.Length -lt 2) {
      Write-Host "[Malformed] $p : $line" -ForegroundColor Yellow
      continue
    }
    $hash = $parts[0]; $file = $parts[1]
    if (-not (Test-Path $file)) {
      Write-Host "[Missing] $file (declared in $p)" -ForegroundColor Red
      continue
    }
    $calc = (Get-FileHash $file -Algorithm SHA256).Hash.ToLower()
    if ($calc -ne $hash.ToLower()) {
      Write-Host "[Hash Mismatch] $file" -ForegroundColor Red
    } else {
      Write-Host "[OK] $file" -ForegroundColor Green
    }
  }
}

Write-Host "Snapshot verification finished." -ForegroundColor Green
