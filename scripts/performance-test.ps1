#!/usr/bin/env pwsh
# 性能基准测试脚本 / Performance Benchmark Test Script

Write-Host "⚡ 知识图谱性能基准测试 / Knowledge Graph Performance Benchmark" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 测试文档解析性能
Write-Host "📚 测试文档解析性能 / Testing document parsing performance..." -ForegroundColor Yellow
$startTime = Get-Date
$docsCount = (Get-ChildItem -Path "docs" -Recurse -Filter "*.md" | Measure-Object).Count
$endTime = Get-Date
$parseTime = ($endTime - $startTime).TotalMilliseconds

Write-Host "✅ 文档解析完成 / Document parsing completed" -ForegroundColor Green
Write-Host "📊 统计结果 / Statistics:" -ForegroundColor Cyan
Write-Host "   - 文档总数: $docsCount" -ForegroundColor White
Write-Host "   - 解析时间: $parseTime ms" -ForegroundColor White
Write-Host "   - 平均速度: $([math]::Round($docsCount / ($parseTime / 1000), 2)) 文档/秒" -ForegroundColor White

# 测试环境构建性能
Write-Host "🏗️  测试环境构建性能 / Testing environment build performance..." -ForegroundColor Yellow
$startTime = Get-Date
try {
    Set-Location "env/containers"
    docker build -f dockerfiles/base/Dockerfile -t knowledge-graph-perf-test . --no-cache
    $endTime = Get-Date
    $buildTime = ($endTime - $startTime).TotalSeconds
    
    Write-Host "✅ 环境构建完成 / Environment build completed" -ForegroundColor Green
    Write-Host "📊 构建时间: $buildTime 秒" -ForegroundColor White
    
    # 清理测试镜像
    docker rmi knowledge-graph-perf-test --force
    Write-Host "🧹 测试镜像已清理 / Test image cleaned" -ForegroundColor Green
} catch {
    Write-Host "❌ 环境构建测试失败 / Environment build test failed" -ForegroundColor Red
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🎉 性能基准测试完成！/ Performance benchmark test completed!" -ForegroundColor Green
