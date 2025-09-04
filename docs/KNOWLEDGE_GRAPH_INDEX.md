# 知识图谱项目索引 / Knowledge Graph Project Index

## 1. 核心模块导航 / Core Modules Navigation

### 1.1 知识表示 / Knowledge Representation

- **文档**: [01-knowledge-representation/README.md](01-knowledge-representation/README.md)
- **示例评测**: [evaluation-reports/01-knowledge-representation-sample.md](evaluation-reports/01-knowledge-representation-sample.md)
- **环境容器**: `ghcr.io/kg/kr-eval:1.0.0`

### 1.2 图论 / Graph Theory

- **文档**: [02-graph-theory/README.md](02-graph-theory/README.md)
- **示例评测**: [evaluation-reports/02-graph-theory-sample.md](evaluation-reports/02-graph-theory-sample.md)
- **环境容器**: `ghcr.io/kg/gt-eval:1.0.0`

### 1.3 语义分析 / Semantic Analysis

- **文档**: [03-semantic-analysis/README.md](03-semantic-analysis/README.md)
- **示例评测**: [evaluation-reports/03-semantic-analysis-sample.md](evaluation-reports/03-semantic-analysis-sample.md)
- **环境容器**: `ghcr.io/kg/sa-eval:1.0.0`

### 1.4 本体工程 / Ontology Engineering

- **文档**: [04-ontology-engineering/README.md](04-ontology-engineering/README.md)
- **示例评测**: [evaluation-reports/04-ontology-engineering-sample.md](evaluation-reports/04-ontology-engineering-sample.md)
- **环境容器**: `ghcr.io/kg/oe-eval:1.0.0`

### 1.5 知识抽取 / Knowledge Extraction

- **文档**: [05-knowledge-extraction/README.md](05-knowledge-extraction/README.md)
- **示例评测**: [evaluation-reports/05-knowledge-extraction-sample.md](evaluation-reports/05-knowledge-extraction-sample.md)
- **环境容器**: `ghcr.io/kg/ke-eval:1.0.0`

### 1.6 推理系统 / Reasoning Systems

- **文档**: [06-reasoning-systems/README.md](06-reasoning-systems/README.md)
- **示例评测**: [evaluation-reports/06-reasoning-systems-sample.md](evaluation-reports/06-reasoning-systems-sample.md)
- **环境容器**: `ghcr.io/kg/rs-eval:1.0.0`

### 1.7 应用 / Applications

- **文档**: [07-applications/README.md](07-applications/README.md)
- **示例评测**: [evaluation-reports/07-applications-sample.md](evaluation-reports/07-applications-sample.md)
- **环境容器**: `ghcr.io/kg/app-eval:1.0.0`

### 1.8 形式化方法 / Formal Methods

- **文档**: [08-formal-methods/README.md](08-formal-methods/README.md)
- **示例评测**: [evaluation-reports/08-formal-methods-sample.md](evaluation-reports/08-formal-methods-sample.md)
- **环境容器**: `ghcr.io/kg/fm-eval:1.0.0`

### 1.9 工程实践 / Engineering Practice

- **文档**: [09-engineering-practice/README.md](09-engineering-practice/README.md)
- **示例评测**: [evaluation-reports/09-engineering-practice-sample.md](evaluation-reports/09-engineering-practice-sample.md)
- **环境容器**: `ghcr.io/kg/ep-eval:1.0.0`

### 1.10 研究方法论 / Research Methodology

- **文档**: [10-research-methodology/README.md](10-research-methodology/README.md)
- **示例评测**: [evaluation-reports/10-research-methodology-sample.md](evaluation-reports/10-research-methodology-sample.md)
- **环境容器**: `ghcr.io/kg/rm-eval:1.0.0`

## 2. 工具与脚本导航 / Tools and Scripts Navigation

### 2.1 文档检查工具 / Documentation Check Tools

- **文档检查脚本**: [tools/docs-check.ps1](tools/docs-check.ps1)
- **快照校验脚本**: [tools/snapshot-verify.ps1](tools/snapshot-verify.ps1)

### 2.2 环境管理工具 / Environment Management Tools

- **Linux构建脚本**: [env/containers/scripts/build-all.sh](env/containers/scripts/build-all.sh)
- **Windows构建脚本**: [env/containers/scripts/build-all.ps1](env/containers/scripts/build-all.ps1)
- **Docker Compose配置**: [env/containers/docker-compose/evaluation.yml](env/containers/docker-compose/evaluation.yml)

## 3. 数据与快照导航 / Data and Snapshots Navigation

### 3.1 数据快照 / Data Snapshots

- **快照说明**: [data/snapshots/README.md](../../data/snapshots/README.md)
- **数据集分类**: 知识表示、图论、语义分析等10个类别
- **校验文件**: SHA256格式，支持完整性验证

### 3.2 环境容器 / Environment Containers

- **容器说明**: [env/containers/README.md](../../env/containers/README.md)
- **Dockerfile**: 基础镜像 + 11个专业环境
- **容器标签**: 统一命名规范 `ghcr.io/kg/{module}-eval:1.0.0`

## 4. 评估框架导航 / Evaluation Framework Navigation

### 4.1 评估报告模板 / Evaluation Report Template

- **统一模板**: [evaluation-report-template.md](evaluation-report-template.md)
- **标准格式**: 元信息、范围、环境、过程、结果、对比、结论、复现

### 4.2 示例评测报告 / Sample Evaluation Reports

- **完整示例**: 10个模块的完整评测报告示例
- **最小可运行示例**: 包含代码片段和表格示例
- **图与公式编号**: 标准化的编号规范

## 5. 标准与规范导航 / Standards and Specifications Navigation

### 5.1 文档标准 / Documentation Standards

- **文档规范**: [DOCUMENTATION_STANDARDS.md](DOCUMENTATION_STANDARDS.md)
- **模板文件**: [template.md](template.md)
- **术语词典**: [terminology-dictionary.md](terminology-dictionary.md)
- **使用指南**: [USER_GUIDE.md](USER_GUIDE.md)
- **贡献指南**: [CONTRIBUTING.md](CONTRIBUTING.md)

### 5.2 学术引用规范 / Academic Citation Standards

- **引用标准**: [ACADEMIC_CITATION_STANDARDS.md](ACADEMIC_CITATION_STANDARDS.md)
- **交叉引用**: 内部链接和外部引用格式

## 6. 快速开始 / Quick Start

### 6.1 环境搭建 / Environment Setup

```bash
# 克隆项目
git clone <repository-url>
cd KnowledgeGraph

# 构建环境（Linux/macOS）
bash env/containers/scripts/build-all.sh

# 构建环境（Windows）
powershell -ExecutionPolicy Bypass -File env/containers/scripts/build-all.ps1

# 启动评测环境
docker-compose -f env/containers/docker-compose/evaluation.yml up -d
```

### 6.2 文档检查 / Documentation Check

```bash
# 检查文档一致性
powershell -ExecutionPolicy Bypass -File docs/tools/docs-check.ps1

# 验证快照完整性
powershell -ExecutionPolicy Bypass -File docs/tools/snapshot-verify.ps1 -SnapshotDir ./data/snapshots
```

### 6.3 运行评测 / Run Evaluation

```bash
# 进入特定环境容器
docker exec -it kg-kr-eval bash

# 运行评测脚本
bash scripts/kr_eval.sh
```

## 7. 项目结构 / Project Structure

```text
KnowledgeGraph/
├── docs/                          # 文档目录
│   ├── 01-knowledge-representation/  # 知识表示
│   ├── 02-graph-theory/             # 图论
│   ├── 03-semantic-analysis/        # 语义分析
│   ├── 04-ontology-engineering/     # 本体工程
│   ├── 05-knowledge-extraction/     # 知识抽取
│   ├── 06-reasoning-systems/        # 推理系统
│   ├── 07-applications/             # 应用
│   ├── 08-formal-methods/           # 形式化方法
│   ├── 09-engineering-practice/     # 工程实践
│   ├── 10-research-methodology/     # 研究方法论
│   ├── evaluation-reports/           # 评测报告
│   └── tools/                        # 工具脚本
├── data/                          # 数据目录
│   └── snapshots/                 # 数据快照
├── env/                           # 环境目录
│   └── containers/                # 容器配置
└── scripts/                       # 运行脚本
```

## 8. 项目状态与报告 / Project Status and Reports

### 8.1 项目里程碑 / Project Milestone

- **[项目里程碑报告](../../PROJECT_MILESTONE_REPORT.md)**: 🎉 项目里程碑达成报告，92%完成度
- **[项目状态报告](../../PROJECT_STATUS.md)**: 详细的项目完成状态和架构说明
- **[项目概述](../../ai.md)**: 项目的核心需求和目标说明

## 9. 评估与协议导航 / Evaluation and Protocol Navigation

### 9.1 快速访问锚点 / Quick Access Anchors

每个模块文档都包含以下标准章节的快速链接：

- **[评估与基准 / Evaluation & Benchmarks](#评估与基准--evaluation--benchmarks)**
- **[统一评测协议 / Unified Evaluation Protocol](#统一评测协议--unified-evaluation-protocol)**
- **[交叉引用与导航 / Cross-referencing and Navigation](#交叉引用与导航--cross-referencing-and-navigation)**

### 9.2 评估报告导航 / Evaluation Report Navigation

- **统一模板**: [evaluation-report-template.md](evaluation-report-template.md)
- **示例报告**: [evaluation-reports/](evaluation-reports/)
- **最小可运行示例**: 所有示例报告都包含代码片段和表格示例

### 9.3 环境与数据导航 / Environment and Data Navigation

- **容器环境**: [env/containers/README.md](../../env/containers/README.md)
- **数据快照**: [data/snapshots/README.md](../../data/snapshots/README.md)
- **构建脚本**: [env/containers/scripts/](../../env/containers/scripts/)

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0
**维护者** / Maintainer: KnowledgeGraph Team
