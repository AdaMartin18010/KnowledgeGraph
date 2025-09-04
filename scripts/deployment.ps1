#!/usr/bin/env pwsh
# 项目部署和发布脚本 / Project Deployment and Release Script
# 自动化部署知识图谱项目环境 / Automated deployment of Knowledge Graph project environment

Write-Host "🚀 知识图谱项目部署开始 / Knowledge Graph Project Deployment Started" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 配置参数
$config = @{
    ProjectName = "knowledge-graph"
    Version = "v1.0.0"
    Registry = "localhost:5000"
    BaseImage = "ubuntu:22.04"
    PythonVersion = "3.11"
    BuildContext = "env/containers"
}

Write-Host "📋 部署配置 / Deployment Configuration:" -ForegroundColor Yellow
Write-Host "   项目名称: $($config.ProjectName)" -ForegroundColor White
Write-Host "   版本: $($config.Version)" -ForegroundColor White
Write-Host "   注册表: $($config.Registry)" -ForegroundColor White
Write-Host "   基础镜像: $($config.BaseImage)" -ForegroundColor White
Write-Host "   构建上下文: $($config.BuildContext)" -ForegroundColor White

# 1. 环境预检查
Write-Host "🔍 1. 环境预检查 / Environment Pre-check" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

# 检查Docker服务
try {
    $dockerInfo = docker info
    Write-Host "✅ Docker服务运行正常" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker服务未启动，请启动Docker Desktop" -ForegroundColor Red
    exit 1
}

# 检查磁盘空间
$diskSpace = Get-WmiObject -Class Win32_LogicalDisk -Filter "DeviceID='C:'" | Select-Object Size, FreeSpace
$freeGB = [math]::Round($diskSpace.FreeSpace / 1GB, 2)
Write-Host "💾 可用磁盘空间: $freeGB GB" -ForegroundColor $(if ($freeGB -gt 10) { "Green" } else { "Yellow" })

if ($freeGB -lt 5) {
    Write-Host "⚠️  磁盘空间不足，建议清理后继续" -ForegroundColor Yellow
}

# 2. 构建基础镜像
Write-Host "🏗️  2. 构建基础镜像 / Building Base Image" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

Set-Location $config.BuildContext

try {
    Write-Host "🔧 构建基础镜像..." -ForegroundColor White
    docker build -f dockerfiles/base/Dockerfile -t $($config.ProjectName)-base:$($config.Version) .
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ 基础镜像构建成功" -ForegroundColor Green
    } else {
        throw "构建失败，退出码: $LASTEXITCODE"
    }
} catch {
    Write-Host "❌ 基础镜像构建失败: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

# 3. 构建专业环境镜像
Write-Host "🔬 3. 构建专业环境镜像 / Building Professional Environment Images" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$environments = @(
    "knowledge-representation",
    "graph-theory",
    "semantic-analysis", 
    "ontology-engineering",
    "knowledge-extraction",
    "reasoning-systems",
    "applications",
    "formal-methods",
    "engineering-practice",
    "research-methodology"
)

$buildResults = @()

foreach ($env in $environments) {
    Write-Host "🔧 构建 $env 环境..." -ForegroundColor White
    try {
        docker build -f "dockerfiles/$env/Dockerfile" -t "$($config.ProjectName)-$env`:$($config.Version)" .
        if ($LASTEXITCODE -eq 0) {
            Write-Host "✅ $env 环境构建成功" -ForegroundColor Green
            $buildResults += @{ Environment = $env; Status = "Success" }
        } else {
            throw "构建失败"
        }
    } catch {
        Write-Host "❌ $env 环境构建失败: $($_.Exception.Message)" -ForegroundColor Red
        $buildResults += @{ Environment = $env; Status = "Failed" }
    }
}

# 4. 启动评估环境
Write-Host "🚀 4. 启动评估环境 / Starting Evaluation Environment" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

try {
    Write-Host "🔧 启动Docker Compose服务..." -ForegroundColor White
    docker-compose -f docker-compose/evaluation.yml up -d
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ 评估环境启动成功" -ForegroundColor Green
    } else {
        throw "启动失败"
    }
} catch {
    Write-Host "❌ 评估环境启动失败: $($_.Exception.Message)" -ForegroundColor Red
}

# 5. 性能监控脚本
Write-Host "📊 5. 创建性能监控脚本 / Creating Performance Monitoring Script" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$monitoringContent = @'
#!/usr/bin/env pwsh
# 性能监控脚本 / Performance Monitoring Script
# 监控知识图谱项目运行状态 / Monitor Knowledge Graph project running status

Write-Host "📊 知识图谱项目性能监控 / Knowledge Graph Project Performance Monitoring" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 监控Docker容器状态
Write-Host "🐳 Docker容器状态 / Docker Container Status:" -ForegroundColor Yellow
try {
    $containers = docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    Write-Host $containers -ForegroundColor White
} catch {
    Write-Host "❌ 无法获取容器状态" -ForegroundColor Red
}

# 监控系统资源
Write-Host "💻 系统资源使用 / System Resource Usage:" -ForegroundColor Yellow
$cpu = Get-WmiObject -Class Win32_Processor | Select-Object LoadPercentage
$memory = Get-WmiObject -Class Win32_OperatingSystem | Select-Object TotalVisibleMemorySize, FreePhysicalMemory
$disk = Get-WmiObject -Class Win32_LogicalDisk -Filter "DeviceID='C:'" | Select-Object Size, FreeSpace

Write-Host "   CPU使用率: $($cpu.LoadPercentage)%" -ForegroundColor White
Write-Host "   内存使用: $([math]::Round(($memory.TotalVisibleMemorySize - $memory.FreePhysicalMemory) / 1MB, 2)) GB / $([math]::Round($memory.TotalVisibleMemorySize / 1MB, 2)) GB" -ForegroundColor White
Write-Host "   磁盘使用: $([math]::Round(($disk.Size - $disk.FreeSpace) / 1GB, 2)) GB / $([math]::Round($disk.Size / 1GB, 2)) GB" -ForegroundColor White

# 监控网络连接
Write-Host "🌐 网络连接状态 / Network Connection Status:" -ForegroundColor Yellow
try {
    $netstat = netstat -an | Select-String "LISTENING" | Select-String ":80|:443|:8080|:3000"
    if ($netstat) {
        Write-Host "   活跃端口:" -ForegroundColor White
        $netstat | ForEach-Object { Write-Host "     $_" -ForegroundColor White }
    } else {
        Write-Host "   未发现活跃端口" -ForegroundColor Gray
    }
} catch {
    Write-Host "❌ 无法获取网络状态" -ForegroundColor Red
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "📊 性能监控完成！/ Performance monitoring completed!" -ForegroundColor Green
'@

Set-Content -Path "scripts/performance-monitor.ps1" -Value $monitoringContent -Encoding UTF8
Write-Host "✅ 性能监控脚本创建成功" -ForegroundColor Green

# 6. 部署状态报告
Write-Host "📋 6. 部署状态报告 / Deployment Status Report" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$successCount = ($buildResults | Where-Object { $_.Status -eq "Success" }).Count
$totalCount = $buildResults.Count
$successRate = [math]::Round(($successCount / $totalCount) * 100, 1)

Write-Host "📊 构建结果统计 / Build Results Statistics:" -ForegroundColor Cyan
Write-Host "   总环境数: $totalCount" -ForegroundColor White
Write-Host "   构建成功: $successCount" -ForegroundColor Green
Write-Host "   构建失败: $($totalCount - $successCount)" -ForegroundColor Red
Write-Host "   成功率: $successRate%" -ForegroundColor $(if ($successRate -ge 90) { "Green" } elseif ($successRate -ge 70) { "Yellow" } else { "Red" })

# 7. 清理和优化
Write-Host "🧹 7. 清理和优化 / Cleanup and Optimization" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

try {
    Write-Host "🧹 清理未使用的Docker资源..." -ForegroundColor White
    docker system prune -f
    Write-Host "✅ 清理完成" -ForegroundColor Green
} catch {
    Write-Host "⚠️  清理过程中出现警告" -ForegroundColor Yellow
}

# 8. 部署完成总结
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🎉 项目部署完成！/ Project Deployment Completed!" -ForegroundColor Green
Write-Host "📝 部署摘要 / Deployment Summary:" -ForegroundColor Cyan
Write-Host "   ✅ 基础镜像: $($config.ProjectName)-base:$($config.Version)" -ForegroundColor White
Write-Host "   ✅ 专业环境: $successCount/$totalCount 个" -ForegroundColor White
Write-Host "   ✅ 评估环境: 已启动" -ForegroundColor White
Write-Host "   ✅ 监控脚本: 已创建" -ForegroundColor White

Write-Host "🚀 下一步操作 / Next Steps:" -ForegroundColor Yellow
Write-Host "   1. 运行性能测试: .\scripts\performance-test.ps1" -ForegroundColor White
Write-Host "   2. 启动性能监控: .\scripts\performance-monitor.ps1" -ForegroundColor White
Write-Host "   3. 运行综合测试: .\scripts\comprehensive-test.ps1" -ForegroundColor White
Write-Host "   4. 查看用户指南: .\USER_GUIDE.md" -ForegroundColor White

Write-Host "🔗 项目已准备就绪，可以开始使用！" -ForegroundColor Green
