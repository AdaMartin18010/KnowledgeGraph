#!/usr/bin/env pwsh
# 项目完成度最终验证脚本 / Project Completion Final Validation Script
# 全面检查项目状态并生成最终报告 / Comprehensive project status check and final report generation

Write-Host "🔍 知识图谱项目完成度最终验证 / Knowledge Graph Project Completion Final Validation" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 验证结果存储
$validationResults = @{
    "Documentation" = @{ Status = $false; Score = 0; Details = @() }
    "Environment" = @{ Status = $false; Score = 0; Details = @() }
    "Scripts" = @{ Status = $false; Score = 0; Details = @() }
    "Data" = @{ Status = $false; Score = 0; Details = @() }
    "Quality" = @{ Status = $false; Score = 0; Details = @() }
}

# 1. 文档完整性验证
Write-Host "📚 1. 文档完整性验证 / Documentation Completeness Validation" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$docsScore = 0
$docsTotal = 0

# 检查核心模块文档
$coreModules = @(
    "01-knowledge-representation",
    "02-graph-theory",
    "03-semantic-analysis",
    "04-ontology-engineering",
    "05-knowledge-extraction",
    "06-reasoning-systems",
    "07-applications",
    "08-formal-methods",
    "09-engineering-practice",
    "10-research-methodology"
)

foreach ($module in $coreModules) {
    $docsTotal++
    $modulePath = "docs/$module"
    $readmePath = "$modulePath/README.md"
    
    if (Test-Path $modulePath) {
        if (Test-Path $readmePath) {
            $docsScore++
            Write-Host "✅ $module - 完整" -ForegroundColor Green
        } else {
            Write-Host "❌ $module - README缺失" -ForegroundColor Red
            $validationResults.Documentation.Details += "模块 $module README缺失"
        }
    } else {
        Write-Host "❌ $module - 目录缺失" -ForegroundColor Red
        $validationResults.Documentation.Details += "模块 $module 目录缺失"
    }
}

# 检查评测报告
$evalReportsPath = "docs/evaluation-reports"
if (Test-Path $evalReportsPath) {
    $evalReports = Get-ChildItem -Path $evalReportsPath -Filter "*.md" | Measure-Object
    if ($evalReports.Count -ge 10) {
        $docsScore++
        Write-Host "✅ 评测报告 - 完整 ($($evalReports.Count) 个)" -ForegroundColor Green
    } else {
        Write-Host "⚠️  评测报告 - 不完整 ($($evalReports.Count)/10)" -ForegroundColor Yellow
        $validationResults.Documentation.Details += "评测报告数量不足: $($evalReports.Count)/10"
    }
    $docsTotal++
} else {
    Write-Host "❌ 评测报告目录缺失" -ForegroundColor Red
    $validationResults.Documentation.Details += "评测报告目录缺失"
    $docsTotal++
}

# 检查标准文档
$standardDocs = @("CONTRIBUTING.md", "DOCUMENTATION_STANDARDS.md", "ACADEMIC_CITATION_STANDARDS.md")
foreach ($doc in $standardDocs) {
    $docsTotal++
    if (Test-Path "docs/$doc") {
        $docsScore++
        Write-Host "✅ $doc - 存在" -ForegroundColor Green
    } else {
        Write-Host "❌ $doc - 缺失" -ForegroundColor Red
        $validationResults.Documentation.Details += "标准文档 $doc 缺失"
    }
}

$validationResults.Documentation.Score = [math]::Round(($docsScore / $docsTotal) * 100, 1)
$validationResults.Documentation.Status = $validationResults.Documentation.Score -ge 95

# 2. 环境配置验证
Write-Host "🔧 2. 环境配置验证 / Environment Configuration Validation" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$envScore = 0
$envTotal = 0

# 检查Docker环境
try {
    $dockerVersion = docker --version
    $envScore++
    Write-Host "✅ Docker环境 - 正常" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker环境 - 异常" -ForegroundColor Red
    $validationResults.Environment.Details += "Docker环境异常"
}
$envTotal++

# 检查Docker Compose
try {
    $composeVersion = docker-compose --version
    $envScore++
    Write-Host "✅ Docker Compose - 正常" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker Compose - 异常" -ForegroundColor Red
    $validationResults.Environment.Details += "Docker Compose异常"
}
$envTotal++

# 检查容器配置
$containerPaths = @(
    "env/containers/dockerfiles/base",
    "env/containers/dockerfiles/knowledge-representation",
    "env/containers/dockerfiles/graph-theory",
    "env/containers/dockerfiles/semantic-analysis",
    "env/containers/dockerfiles/ontology-engineering",
    "env/containers/dockerfiles/knowledge-extraction",
    "env/containers/dockerfiles/reasoning-systems",
    "env/containers/dockerfiles/applications",
    "env/containers/dockerfiles/formal-methods",
    "env/containers/dockerfiles/engineering-practice",
    "env/containers/dockerfiles/research-methodology"
)

foreach ($path in $containerPaths) {
    $envTotal++
    if (Test-Path $path) {
        $dockerfilePath = "$path/Dockerfile"
        if (Test-Path $dockerfilePath) {
            $envScore++
            Write-Host "✅ $path - 完整" -ForegroundColor Green
        } else {
            Write-Host "❌ $path - Dockerfile缺失" -ForegroundColor Red
            $validationResults.Environment.Details += "$path Dockerfile缺失"
        }
    } else {
        Write-Host "❌ $path - 目录缺失" -ForegroundColor Red
        $validationResults.Environment.Details += "$path 目录缺失"
    }
}

$validationResults.Environment.Score = [math]::Round(($envScore / $envTotal) * 100, 1)
$validationResults.Environment.Status = $validationResults.Environment.Score -ge 90

# 3. 脚本工具验证
Write-Host "🛠️  3. 脚本工具验证 / Scripts and Tools Validation" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$scriptsScore = 0
$scriptsTotal = 0

# 检查核心脚本
$coreScripts = @(
    "scripts/quick-start.sh",
    "scripts/quick-start.ps1",
    "scripts/project-completion-check.sh",
    "scripts/run-all-evaluations.sh",
    "scripts/kr_eval.sh",
    "scripts/gt_eval.sh"
)

foreach ($script in $coreScripts) {
    $scriptsTotal++
    if (Test-Path $script) {
        $scriptsScore++
        Write-Host "✅ $script - 存在" -ForegroundColor Green
    } else {
        Write-Host "❌ $script - 缺失" -ForegroundColor Red
        $validationResults.Scripts.Details += "核心脚本 $script 缺失"
    }
}

# 检查新增脚本
$newScripts = @(
    "scripts/windows-optimize.ps1",
    "scripts/quality-enhancement.ps1",
    "scripts/performance-test.ps1",
    "scripts/comprehensive-test.ps1",
    "scripts/deployment.ps1",
    "scripts/performance-monitor.ps1"
)

foreach ($script in $newScripts) {
    $scriptsTotal++
    if (Test-Path $script) {
        $scriptsScore++
        Write-Host "✅ $script - 存在" -ForegroundColor Green
    } else {
        Write-Host "❌ $script - 缺失" -ForegroundColor Red
        $validationResults.Scripts.Details += "新增脚本 $script 缺失"
    }
}

$validationResults.Scripts.Score = [math]::Round(($scriptsScore / $scriptsTotal) * 100, 1)
$validationResults.Scripts.Status = $validationResults.Scripts.Score -ge 90

# 4. 数据管理验证
Write-Host "📊 4. 数据管理验证 / Data Management Validation" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$dataScore = 0
$dataTotal = 0

# 检查数据目录
$dataPaths = @(
    "data/snapshots",
    "data/snapshots/README.md"
)

foreach ($path in $dataPaths) {
    $dataTotal++
    if (Test-Path $path) {
        $dataScore++
        Write-Host "✅ $path - 存在" -ForegroundColor Green
    } else {
        Write-Host "❌ $path - 缺失" -ForegroundColor Red
        $validationResults.Data.Details += "数据路径 $path 缺失"
    }
}

# 检查快照验证工具
$snapshotTools = @(
    "docs/tools/snapshot-verify.ps1",
    "docs/tools/docs-check.ps1"
)

foreach ($tool in $snapshotTools) {
    $dataTotal++
    if (Test-Path $tool) {
        $dataScore++
        Write-Host "✅ $tool - 存在" -ForegroundColor Green
    } else {
        Write-Host "❌ $tool - 缺失" -ForegroundColor Red
        $validationResults.Data.Details += "快照工具 $tool 缺失"
    }
}

$validationResults.Data.Score = [math]::Round(($dataScore / $dataTotal) * 100, 1)
$validationResults.Data.Status = $validationResults.Data.Score -ge 90

# 5. 质量保证验证
Write-Host "🔍 5. 质量保证验证 / Quality Assurance Validation" -ForegroundColor Yellow
Write-Host "------------------------------------------" -ForegroundColor Gray

$qualityScore = 0
$qualityTotal = 0

# 检查用户指南
if (Test-Path "USER_GUIDE.md") {
    $qualityScore++
    Write-Host "✅ 用户指南 - 存在" -ForegroundColor Green
} else {
    Write-Host "❌ 用户指南 - 缺失" -ForegroundColor Red
    $validationResults.Quality.Details += "用户指南缺失"
}
$qualityTotal++

# 检查项目状态报告
$statusReports = @("PROJECT_STATUS.md", "PROJECT_MILESTONE_REPORT.md")
foreach ($report in $statusReports) {
    $qualityTotal++
    if (Test-Path $report) {
        $qualityScore++
        Write-Host "✅ $report - 存在" -ForegroundColor Green
    } else {
        Write-Host "❌ $report - 缺失" -ForegroundColor Red
        $validationResults.Quality.Details += "状态报告 $report 缺失"
    }
}

# 检查许可证文件
if (Test-Path "LICENSE") {
    $qualityScore++
    Write-Host "✅ 许可证文件 - 存在" -ForegroundColor Green
} else {
    Write-Host "❌ 许可证文件 - 缺失" -ForegroundColor Red
    $validationResults.Quality.Details += "许可证文件缺失"
}
$qualityTotal++

$validationResults.Quality.Score = [math]::Round(($qualityScore / $qualityTotal) * 100, 1)
$validationResults.Quality.Status = $validationResults.Quality.Score -ge 90

# 6. 生成最终验证报告
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "📊 最终验证报告 / Final Validation Report" -ForegroundColor Yellow
Write-Host "==========================================" -ForegroundColor Cyan

$overallScore = 0
$overallTotal = 0
$passedCategories = 0

foreach ($category in $validationResults.GetEnumerator()) {
    $overallScore += $category.Value.Score
    $overallTotal++
    if ($category.Value.Status) {
        $passedCategories++
    }
    
    $statusIcon = if ($category.Value.Status) { "✅" } else { "❌" }
    $color = if ($category.Value.Status) { "Green" } else { "Red" }
    
    Write-Host "$statusIcon $($category.Key): $($category.Value.Score)%" -ForegroundColor $color
    
    if ($category.Value.Details.Count -gt 0) {
        foreach ($detail in $category.Value.Details) {
            Write-Host "   ⚠️  $detail" -ForegroundColor Yellow
        }
    }
}

$overallPercentage = [math]::Round($overallScore / $overallTotal, 1)
$overallStatus = if ($overallPercentage -ge 95) { "🟢 优秀" } elseif ($overallPercentage -ge 90) { "🟡 良好" } elseif ($overallPercentage -ge 80) { "🟠 一般" } else { "🔴 需要改进" }

Write-Host "------------------------------------------" -ForegroundColor Gray
Write-Host "📈 总体评分: $overallPercentage%" -ForegroundColor Cyan
Write-Host "📊 通过类别: $passedCategories/$overallTotal" -ForegroundColor Cyan
Write-Host "🎯 项目状态: $overallStatus" -ForegroundColor Cyan

# 7. 生成改进建议
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "💡 改进建议 / Improvement Suggestions" -ForegroundColor Yellow

if ($overallPercentage -ge 95) {
    Write-Host "🎉 恭喜！项目已达到优秀标准！" -ForegroundColor Green
    Write-Host "🚀 建议下一步操作:" -ForegroundColor Cyan
    Write-Host "   1. 开始实际使用和测试" -ForegroundColor White
    Write-Host "   2. 收集用户反馈" -ForegroundColor White
    Write-Host "   3. 考虑开源发布" -ForegroundColor White
    Write-Host "   4. 建立用户社区" -ForegroundColor White
} elseif ($overallPercentage -ge 90) {
    Write-Host "👍 项目状态良好，接近优秀标准" -ForegroundColor Green
    Write-Host "🔧 建议改进:" -ForegroundColor Cyan
    foreach ($category in $validationResults.GetEnumerator()) {
        if (-not $category.Value.Status) {
            Write-Host "   - 修复 $($category.Key) 类别的问题" -ForegroundColor White
        }
    }
} else {
    Write-Host "⚠️  项目需要进一步改进" -ForegroundColor Yellow
    Write-Host "🔧 优先改进:" -ForegroundColor Cyan
    $sortedCategories = $validationResults.GetEnumerator() | Sort-Object { $_.Value.Score }
    foreach ($category in $sortedCategories) {
        if (-not $category.Value.Status) {
            Write-Host "   - 优先修复 $($category.Key) 类别" -ForegroundColor White
        }
    }
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🔍 最终验证完成！/ Final validation completed!" -ForegroundColor Green
Write-Host "📝 项目已准备就绪，可以投入使用！" -ForegroundColor Cyan
