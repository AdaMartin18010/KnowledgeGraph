#!/usr/bin/env pwsh
# Windows环境检查脚本 / Windows Environment Check Script

Write-Host "🔍 Windows环境检查 / Windows Environment Check" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 检查PowerShell版本
Write-Host "📋 PowerShell版本 / PowerShell Version:" -ForegroundColor Yellow
$PSVersionTable.PSVersion

# 检查.NET版本
Write-Host "📋 .NET版本 / .NET Version:" -ForegroundColor Yellow
try {
    $dotnetVersion = dotnet --version
    Write-Host "✅ .NET: $dotnetVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ .NET未安装 / .NET not installed" -ForegroundColor Red
}

# 检查Git版本
Write-Host "📋 Git版本 / Git Version:" -ForegroundColor Yellow
try {
    $gitVersion = git --version
    Write-Host "✅ Git: $gitVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ Git未安装 / Git not installed" -ForegroundColor Red
}

# 检查Docker版本
Write-Host "📋 Docker版本 / Docker Version:" -ForegroundColor Yellow
try {
    $dockerVersion = docker --version
    Write-Host "✅ Docker: $dockerVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker未安装 / Docker not installed" -ForegroundColor Red
}

# 检查WSL状态
Write-Host "📋 WSL状态 / WSL Status:" -ForegroundColor Yellow
try {
    $wslStatus = wsl --status
    Write-Host "✅ WSL: $wslStatus" -ForegroundColor Green
} catch {
    Write-Host "❌ WSL未安装或未启用 / WSL not installed or not enabled" -ForegroundColor Red
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🔍 环境检查完成 / Environment check completed" -ForegroundColor Green
