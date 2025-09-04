#!/usr/bin/env pwsh
# 综合测试脚本 / Comprehensive Test Script
# 运行所有测试和检查 / Run all tests and checks

Write-Host "🧪 知识图谱项目综合测试 / Knowledge Graph Project Comprehensive Test" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

$testResults = @{
    "Document Check" = $false
    "Environment Check" = $false
    "Performance Test" = $false
    "Quality Check" = $false
}

# 1. 文档检查
Write-Host "📚 1. 文档完整性检查 / Document Completeness Check" -ForegroundColor Yellow
try {
    bash scripts/project-completion-check.sh
    $testResults["Document Check"] = $true
    Write-Host "✅ 文档检查通过 / Document check passed" -ForegroundColor Green
} catch {
    Write-Host "❌ 文档检查失败 / Document check failed" -ForegroundColor Red
}

# 2. 环境检查
Write-Host "🔧 2. 环境配置检查 / Environment Configuration Check" -ForegroundColor Yellow
try {
    powershell -ExecutionPolicy Bypass -File scripts/windows-env-check.ps1
    $testResults["Environment Check"] = $true
    Write-Host "✅ 环境检查通过 / Environment check passed" -ForegroundColor Green
} catch {
    Write-Host "❌ 环境检查失败 / Environment check failed" -ForegroundColor Red
}

# 3. 性能测试
Write-Host "⚡ 3. 性能基准测试 / Performance Benchmark Test" -ForegroundColor Yellow
try {
    powershell -ExecutionPolicy Bypass -File scripts/performance-test.ps1
    $testResults["Performance Test"] = $true
    Write-Host "✅ 性能测试通过 / Performance test passed" -ForegroundColor Green
} catch {
    Write-Host "❌ 性能测试失败 / Performance test failed" -ForegroundColor Red
}

# 4. 质量检查
Write-Host "🔍 4. 项目质量检查 / Project Quality Check" -ForegroundColor Yellow
try {
    powershell -ExecutionPolicy Bypass -File docs/tools/docs-check.ps1
    $testResults["Quality Check"] = $true
    Write-Host "✅ 质量检查通过 / Quality check passed" -ForegroundColor Green
} catch {
    Write-Host "❌ 质量检查失败 / Quality check failed" -ForegroundColor Red
}

# 测试结果汇总
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "📊 测试结果汇总 / Test Results Summary" -ForegroundColor Yellow

$passedTests = ($testResults.Values | Where-Object { $_ -eq $true }).Count
$totalTests = $testResults.Count

Write-Host "✅ 通过测试: $passedTests/$totalTests" -ForegroundColor Green
Write-Host "📈 成功率: $([math]::Round($passedTests / $totalTests * 100, 1))%" -ForegroundColor Cyan

foreach ($test in $testResults.GetEnumerator()) {
    $status = if ($test.Value) { "✅ 通过" } else { "❌ 失败" }
    $color = if ($test.Value) { "Green" } else { "Red" }
    Write-Host "   - $($test.Key): $status" -ForegroundColor $color
}

if ($passedTests -eq $totalTests) {
    Write-Host "🎉 所有测试通过！项目质量优秀！" -ForegroundColor Green
} else {
    Write-Host "⚠️  部分测试失败，请检查并修复问题" -ForegroundColor Yellow
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🔍 综合测试完成！/ Comprehensive test completed!" -ForegroundColor Green
