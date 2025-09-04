#!/usr/bin/env pwsh
# Windows环境优化脚本 / Windows Environment Optimization Script
# 解决文件权限警告问题 / Resolve file permission warning issues

Write-Host "🔧 Windows环境优化开始 / Starting Windows Environment Optimization" -ForegroundColor Green

# 设置脚本文件权限为只读
Write-Host "📝 设置脚本文件权限 / Setting script file permissions..." -ForegroundColor Yellow
Get-ChildItem -Path "scripts" -Filter "*.sh" | ForEach-Object {
    try {
        $acl = Get-Acl $_.FullName
        $acl.SetAccessRuleProtection($true, $false)
        $rule = New-Object System.Security.AccessControl.FileSystemAccessRule($env:USERNAME, "Read", "Allow")
        $acl.AddAccessRule($rule)
        Set-Acl $_.FullName $acl
        Write-Host "✅ 已优化: $($_.Name)" -ForegroundColor Green
    }
    catch {
        Write-Host "⚠️  警告: $($_.Name) - $($_.Exception.Message)" -ForegroundColor Yellow
    }
}

# 创建Windows专用的快速启动脚本
Write-Host "🚀 创建Windows快速启动脚本 / Creating Windows quick start script..." -ForegroundColor Yellow
$quickStartContent = @'
#!/usr/bin/env pwsh
# Windows快速启动脚本 / Windows Quick Start Script
# 知识图谱项目环境搭建 / Knowledge Graph Project Environment Setup

Write-Host "🚀 知识图谱项目快速启动 / Knowledge Graph Project Quick Start" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 检查Docker环境
Write-Host "🔍 检查Docker环境 / Checking Docker environment..." -ForegroundColor Yellow
try {
    docker --version | Out-Null
    Write-Host "✅ Docker已安装 / Docker is installed" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker未安装，请先安装Docker Desktop / Docker not installed, please install Docker Desktop first" -ForegroundColor Red
    exit 1
}

# 检查Docker服务状态
Write-Host "🔍 检查Docker服务状态 / Checking Docker service status..." -ForegroundColor Yellow
try {
    docker info | Out-Null
    Write-Host "✅ Docker服务运行正常 / Docker service is running" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker服务未启动，请启动Docker Desktop / Docker service not started, please start Docker Desktop" -ForegroundColor Red
    exit 1
}

# 构建基础环境
Write-Host "🏗️  构建基础环境 / Building base environment..." -ForegroundColor Yellow
try {
    Set-Location "env/containers"
    docker build -f dockerfiles/base/Dockerfile -t knowledge-graph-base .
    Write-Host "✅ 基础环境构建成功 / Base environment built successfully" -ForegroundColor Green
} catch {
    Write-Host "❌ 基础环境构建失败 / Base environment build failed" -ForegroundColor Red
    Write-Host "错误信息 / Error: $($_.Exception.Message)" -ForegroundColor Red
}

# 启动评估环境
Write-Host "🚀 启动评估环境 / Starting evaluation environment..." -ForegroundColor Yellow
try {
    docker-compose -f docker-compose/evaluation.yml up -d
    Write-Host "✅ 评估环境启动成功 / Evaluation environment started successfully" -ForegroundColor Green
} catch {
    Write-Host "❌ 评估环境启动失败 / Evaluation environment start failed" -ForegroundColor Red
    Write-Host "错误信息 / Error: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🎉 环境搭建完成！/ Environment setup completed!" -ForegroundColor Green
Write-Host "📖 请查看docs目录了解详细使用方法 / Please check docs directory for detailed usage" -ForegroundColor Cyan
Write-Host "🔗 项目主页: https://github.com/your-org/knowledge-graph" -ForegroundColor Blue
'@

Set-Content -Path "scripts/windows-quick-start.ps1" -Value $quickStartContent -Encoding UTF8
Write-Host "✅ Windows快速启动脚本创建成功 / Windows quick start script created successfully" -ForegroundColor Green

# 创建Windows环境检查脚本
Write-Host "🔍 创建Windows环境检查脚本 / Creating Windows environment check script..." -ForegroundColor Yellow
$envCheckContent = @'
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
'@

Set-Content -Path "scripts/windows-env-check.ps1" -Value $envCheckContent -Encoding UTF8
Write-Host "✅ Windows环境检查脚本创建成功 / Windows environment check script created successfully" -ForegroundColor Green

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🎉 Windows环境优化完成！/ Windows Environment Optimization Completed!" -ForegroundColor Green
Write-Host "📝 已创建以下脚本 / Created the following scripts:" -ForegroundColor Cyan
Write-Host "   - scripts/windows-quick-start.ps1 (Windows快速启动)" -ForegroundColor White
Write-Host "   - scripts/windows-env-check.ps1 (Windows环境检查)" -ForegroundColor White
Write-Host "🔧 文件权限警告已优化 / File permission warnings optimized" -ForegroundColor Green
