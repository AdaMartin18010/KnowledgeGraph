# 文档标准化规范 / Documentation Standards

## 1. 目录结构规范 / Directory Structure Standards

### 1.1 模块编号规范 / Module Numbering Standards

所有模块必须按照以下格式进行编号：

```text
# [模块编号]. [模块名称] / [Module Name]

## [模块编号].[章节编号] [章节名称] / [Section Name]

### [模块编号].[章节编号].[子章节编号] [子章节名称] / [Subsection Name]

#### [模块编号].[章节编号].[子章节编号].[小节编号] [小节名称] / [Subsection Name]
```

**示例** / Example:

```text
# 01. 知识表示 / Knowledge Representation

## 01.1 概述 / Overview

### 01.1.1 定义与概念 / Definition and Concepts

#### 01.1.1.1 核心定义 / Core Definition
```

### 1.2 章节编号规范 / Section Numbering Standards

每个模块必须包含以下标准章节：

| 章节编号 / Section ID | 章节名称 / Section Name | 中文名称 / Chinese Name | 必需性 / Required |
|---------------------|----------------------|----------------------|-----------------|
| [模块编号].1 | 概述 / Overview | 概述 | ✅ 必需 |
| [模块编号].2 | 理论基础 / Theoretical Foundation | 理论基础 | ✅ 必需 |
| [模块编号].3 | 批判性分析 / Critical Analysis | 批判性分析 | ✅ 必需 |
| [模块编号].4 | 工程实践 / Engineering Practice | 工程实践 | ✅ 必需 |
| [模块编号].5 | 应用领域 / Application Domains | 应用领域 | ✅ 必需 |
| [模块编号].6 | 前沿发展 / Frontier Development | 前沿发展 | ✅ 必需 |
| [模块编号].7 | 总结与展望 / Summary and Prospects | 总结与展望 | ✅ 必需 |
| [模块编号].8 | 参考文献 / References | 参考文献 | ✅ 必需 |
| [模块编号].9 | 相关链接 / Related Links | 相关链接 | ✅ 必需 |

## 2. 内容格式规范 / Content Format Standards

### 2.1 中英文对照规范 / Bilingual Format Standards

所有内容必须包含完整的中英文对照：

```markdown
**中文定义** / Chinese Definition:
[中文内容]

**English Definition:**
[English content]
```

### 2.2 表格格式规范 / Table Format Standards

所有表格必须包含中英文列标题：

```markdown
| 列1 / Column 1 | 列2 / Column 2 | 列3 / Column 3 |
|---------------|---------------|---------------|
| 内容1 / Content 1 | 内容2 / Content 2 | 内容3 / Content 3 |
```

### 2.3 数学公式规范 / Mathematical Formula Standards

数学公式必须使用标准格式：

```markdown
    **数学符号** / Mathematical Notation:

    ```text
    [数学公式]
    ```

    **形式化描述** / Formal Description:
    [详细描述]

```

### 2.4 代码示例规范 / Code Example Standards

代码示例必须包含多种编程语言：

```markdown
```rust
// Rust实现示例
fn example() -> Result<(), Error> {
    // 实现代码
    Ok(())
}
```

```haskell
    -- Haskell实现示例
    example :: IO ()
    example = do
        -- 实现代码
        return ()
    ```

```

## 3. 链接规范 / Link Standards

### 3.1 内部链接规范 / Internal Link Standards

所有内部链接必须使用相对路径：

```markdown
- [相关模块](../related-module/README.md)
- [上级目录](../../README.md)
```

### 3.2 外部链接规范 / External Link Standards

外部链接必须包含访问日期：

```markdown
- [资源名称](https://example.com). Accessed 2024.
```

### 3.3 链接与编号一致性检查清单 / Link & Numbering Consistency Checklist

- [ ] 标题双语且编号连续（如“## 1. 概述 / Overview”）
- [ ] 图/表/公式编号规范（图 2-3 / 表 3-1 / 公式 (4-2)）
- [ ] 内部链接使用相对路径并含锚点（如 `../04-ontology-engineering/README.md#48-统一评测协议--unified-evaluation-protocol`）
- [ ] 外链提供稳定URL或DOI，必要时附访问日期
- [ ] 交叉引用章节与索引锚点一致可达

## 4. 质量保证规范 / Quality Assurance Standards

### 4.1 内容完整性检查 / Content Completeness Check

每个模块必须包含：

- [ ] 完整的章节结构
- [ ] 中英文对照
- [ ] 数学形式化定义
- [ ] 批判性分析
- [ ] 工程实践案例
- [ ] 参考文献
- [ ] 相关链接

### 4.2 格式一致性检查 / Format Consistency Check

每个模块必须符合：

- [ ] 编号规范
- [ ] 表格格式
- [ ] 数学公式格式
- [ ] 代码示例格式
- [ ] 链接格式

### 4.3 学术标准检查 / Academic Standards Check

每个模块必须满足：

- [ ] 引用完整性
- [ ] 证明严谨性
- [ ] 逻辑一致性
- [ ] 内容准确性

### 4.4 评估与协议区块检查清单 / Benchmarks & Protocols Block Checklist

- [ ] 各章包含“评估与基准 / Evaluation & Benchmarks”小节
- [ ] 各章包含“统一评测协议 / Unified Evaluation Protocol”小节
- [ ] 指标维度覆盖：正确性/一致性/完备性/效率/可解释性（按章定制）
- [ ] 基准集合明确且链接可达（优先DOI或稳定URL）
- [ ] 报告格式遵循模板双语编号规范

## 5. 更新维护规范 / Update and Maintenance Standards

### 5.1 版本控制 / Version Control

每个文档必须包含版本信息：

```markdown
**最后更新** / Last Updated: [日期] / [Date]
**版本** / Version: [版本号] / [Version Number]
**维护者** / Maintainer: [维护者] / [Maintainer]
```

### 5.2 更新频率 / Update Frequency

- **内容更新** / Content Updates: 每月检查一次
- **格式优化** / Format Optimization: 每季度检查一次
- **链接验证** / Link Verification: 每半年检查一次

## 6. 自动化检查工具 / Automated Check Tools

### 6.1 格式检查脚本 / Format Check Script

```bash
#!/bin/bash
# 检查文档格式规范
for file in docs/*/README.md; do
    echo "Checking $file..."
    # 检查编号规范
    # 检查表格格式
    # 检查链接格式
done
```

### 6.2 质量评估指标 / Quality Assessment Metrics

- **完整性得分** / Completeness Score: 0-100
- **一致性得分** / Consistency Score: 0-100
- **准确性得分** / Accuracy Score: 0-100
- **可读性得分** / Readability Score: 0-100

### 6.3 PowerShell 一键检查脚本 / PowerShell One-click Check Script

```powershell
# docs-check.ps1
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

# 2) 内部相对链接与锚点基本检查
Get-ChildItem -Recurse "$Root" -Filter README.md | ForEach-Object {
  $p = $_.FullName
  $lines = Get-Content $p
  $i = 0
  foreach ($line in $lines) {
    $i++
    if ($line -match "\]\(\.\./.+README.md#.+\)") {
      # 基础存在性检查（不解析Markdown锚点，仅检查文件是否存在）
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
```

---

**最后更新** / Last Updated: 2024-12-19
**版本** / Version: 1.0.0
**维护者** / Maintainer: Knowledge Graph Team
