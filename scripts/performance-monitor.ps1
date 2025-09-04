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
