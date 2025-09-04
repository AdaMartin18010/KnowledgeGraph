# 知识图谱项目使用指南 / Knowledge Graph Project User Guide

## 🎯 指南概述 / Guide Overview

本指南为知识图谱项目的用户提供详细的使用说明，包括环境搭建、功能使用、问题解决等各个方面。无论您是研究人员、工程师还是学生，都能通过本指南快速上手项目。

## 📋 目录 / Table of Contents

- [知识图谱项目使用指南 / Knowledge Graph Project User Guide](#知识图谱项目使用指南--knowledge-graph-project-user-guide)
  - [🎯 指南概述 / Guide Overview](#-指南概述--guide-overview)
  - [📋 目录 / Table of Contents](#-目录--table-of-contents)
  - [🚀 快速开始 / Quick Start](#-快速开始--quick-start)
    - [系统要求 / System Requirements](#系统要求--system-requirements)
    - [一键启动 / One-Click Start](#一键启动--one-click-start)
      - [Linux/macOS](#linuxmacos)
      - [Windows](#windows)
    - [手动启动 / Manual Start](#手动启动--manual-start)
  - [🐳 环境管理 / Environment Management](#-环境管理--environment-management)
    - [环境概览 / Environment Overview](#环境概览--environment-overview)
    - [环境操作 / Environment Operations](#环境操作--environment-operations)
      - [构建环境 / Build Environment](#构建环境--build-environment)
      - [启动环境 / Start Environment](#启动环境--start-environment)
      - [停止环境 / Stop Environment](#停止环境--stop-environment)
      - [清理环境 / Clean Environment](#清理环境--clean-environment)
  - [🛠️ 功能使用 / Feature Usage](#️-功能使用--feature-usage)
    - [项目管理工具 / Project Management Tools](#项目管理工具--project-management-tools)
    - [文档检查工具 / Documentation Check Tools](#文档检查工具--documentation-check-tools)
      - [文档一致性检查 / Documentation Consistency Check](#文档一致性检查--documentation-consistency-check)
      - [快照完整性验证 / Snapshot Integrity Verification](#快照完整性验证--snapshot-integrity-verification)
    - [快速启动工具 / Quick Start Tools](#快速启动工具--quick-start-tools)
      - [Linux/macOS快速启动 / Linux/macOS Quick Start](#linuxmacos快速启动--linuxmacos-quick-start)
      - [Windows快速启动 / Windows Quick Start](#windows快速启动--windows-quick-start)
  - [🔬 评测运行 / Evaluation Running](#-评测运行--evaluation-running)
    - [综合评测 / Comprehensive Evaluation](#综合评测--comprehensive-evaluation)
    - [模块评测 / Module Evaluation](#模块评测--module-evaluation)
    - [评测结果分析 / Evaluation Result Analysis](#评测结果分析--evaluation-result-analysis)
  - [📊 项目管理 / Project Management](#-项目管理--project-management)
    - [项目状态监控 / Project Status Monitoring](#项目状态监控--project-status-monitoring)
      - [项目完成度检查 / Project Completion Check](#项目完成度检查--project-completion-check)
      - [项目健康检查 / Project Health Check](#项目健康检查--project-health-check)
      - [项目统计报告 / Project Statistics Report](#项目统计报告--project-statistics-report)
    - [项目维护 / Project Maintenance](#项目维护--project-maintenance)
      - [项目备份 / Project Backup](#项目备份--project-backup)
      - [项目恢复 / Project Restore](#项目恢复--project-restore)
      - [项目更新 / Project Update](#项目更新--project-update)
  - [🔧 故障排除 / Troubleshooting](#-故障排除--troubleshooting)
    - [常见问题 / Common Issues](#常见问题--common-issues)
      - [Docker相关问题 / Docker Related Issues](#docker相关问题--docker-related-issues)
      - [脚本执行问题 / Script Execution Issues](#脚本执行问题--script-execution-issues)
    - [日志分析 / Log Analysis](#日志分析--log-analysis)
      - [Docker日志 / Docker Logs](#docker日志--docker-logs)
      - [脚本日志 / Script Logs](#脚本日志--script-logs)
  - [💡 最佳实践 / Best Practices](#-最佳实践--best-practices)
    - [环境管理最佳实践 / Environment Management Best Practices](#环境管理最佳实践--environment-management-best-practices)
    - [评测运行最佳实践 / Evaluation Running Best Practices](#评测运行最佳实践--evaluation-running-best-practices)
    - [项目管理最佳实践 / Project Management Best Practices](#项目管理最佳实践--project-management-best-practices)
  - [❓ 常见问题 / FAQ](#-常见问题--faq)
    - [Q: 如何修改Docker环境配置？](#q-如何修改docker环境配置)
    - [Q: 如何添加新的评测模块？](#q-如何添加新的评测模块)
    - [Q: 如何自定义评测指标？](#q-如何自定义评测指标)
    - [Q: 如何集成外部数据集？](#q-如何集成外部数据集)
    - [Q: 如何扩展项目功能？](#q-如何扩展项目功能)
  - [📞 获取帮助 / Getting Help](#-获取帮助--getting-help)
    - [官方资源 / Official Resources](#官方资源--official-resources)
    - [社区支持 / Community Support](#社区支持--community-support)
    - [联系信息 / Contact Information](#联系信息--contact-information)

## 🚀 快速开始 / Quick Start

### 系统要求 / System Requirements

- **操作系统**: Windows 10+, macOS 10.15+, Ubuntu 18.04+
- **Docker**: Docker Desktop 20.10+ 或 Docker Engine 20.10+
- **Docker Compose**: 1.29+
- **内存**: 建议 8GB+ RAM
- **存储**: 建议 20GB+ 可用空间

### 一键启动 / One-Click Start

#### Linux/macOS

```bash
# 克隆项目
git clone <repository-url>
cd KnowledgeGraph

# 运行快速启动脚本
bash scripts/quick-start.sh
```

#### Windows

```powershell
# 克隆项目
git clone <repository-url>
cd KnowledgeGraph

# 运行快速启动脚本
powershell -ExecutionPolicy Bypass -File scripts/quick-start.ps1
```

### 手动启动 / Manual Start

如果您希望手动控制每个步骤，可以按照以下流程：

```bash
# 1. 构建环境
bash env/containers/scripts/build-all.sh

# 2. 启动评测环境
docker-compose -f env/containers/docker-compose/evaluation.yml up -d

# 3. 进入特定环境
docker exec -it kg-kr-eval bash
```

## 🐳 环境管理 / Environment Management

### 环境概览 / Environment Overview

项目提供了11个专业环境，覆盖知识图谱的各个技术领域：

| 环境名称 | 容器标签 | 主要用途 |
|----------|----------|----------|
| 基础环境 | `ghcr.io/kg/base:latest` | 通用基础环境 |
| 知识表示 | `ghcr.io/kg/kr-eval:1.0.0` | 知识表示学习、嵌入训练 |
| 图论 | `ghcr.io/kg/gt-eval:1.0.0` | 图算法、图神经网络 |
| 语义分析 | `ghcr.io/kg/sa-eval:1.0.0` | NLP、语义理解 |
| 本体工程 | `ghcr.io/kg/oe-eval:1.0.0` | 本体构建、RDF处理 |
| 知识抽取 | `ghcr.io/kg/ke-eval:1.0.0` | 信息抽取、实体识别 |
| 推理系统 | `ghcr.io/kg/rs-eval:1.0.0` | 逻辑推理、规则引擎 |
| 应用 | `ghcr.io/kg/app-eval:1.0.0` | Web应用、API服务 |
| 形式化方法 | `ghcr.io/kg/fm-eval:1.0.0` | 形式化验证、定理证明 |
| 工程实践 | `ghcr.io/kg/ep-eval:1.0.0` | 部署、监控、测试 |
| 研究方法论 | `ghcr.io/kg/rm-eval:1.0.0` | 数据分析、实验设计 |

### 环境操作 / Environment Operations

#### 构建环境 / Build Environment

```bash
# 构建所有环境
bash env/containers/scripts/build-all.sh

# 构建特定环境
docker build -f env/containers/dockerfiles/knowledge-representation/Dockerfile \
  -t ghcr.io/kg/kr-eval:1.0.0 \
  env/containers/dockerfiles/knowledge-representation/
```

#### 启动环境 / Start Environment

```bash
# 启动所有环境
docker-compose -f env/containers/docker-compose/evaluation.yml up -d

# 启动特定环境
docker run -d --name kg-kr-eval ghcr.io/kg/kr-eval:1.0.0
```

#### 停止环境 / Stop Environment

```bash
# 停止所有环境
docker-compose -f env/containers/docker-compose/evaluation.yml down

# 停止特定环境
docker stop kg-kr-eval
```

#### 清理环境 / Clean Environment

```bash
# 清理所有环境
docker-compose -f env/containers/docker-compose/evaluation.yml down -v
docker system prune -f

# 清理特定环境
docker rm -f kg-kr-eval
docker rmi ghcr.io/kg/kr-eval:1.0.0
```

## 🛠️ 功能使用 / Feature Usage

### 项目管理工具 / Project Management Tools

项目提供了综合的管理脚本，支持各种管理操作：

```bash
# 显示帮助信息
bash scripts/project-manager.sh --help

# 显示项目状态
bash scripts/project-manager.sh status

# 运行健康检查
bash scripts/project-manager.sh health

# 生成统计报告
bash scripts/project-manager.sh stats

# 运行完成度检查
bash scripts/project-manager.sh check

# 生成综合报告
bash scripts/project-manager.sh report
```

### 文档检查工具 / Documentation Check Tools

#### 文档一致性检查 / Documentation Consistency Check

```bash
# Windows PowerShell
powershell -ExecutionPolicy Bypass -File docs/tools/docs-check.ps1

# 检查特定目录
powershell -ExecutionPolicy Bypass -File docs/tools/docs-check.ps1 -Path ./docs
```

#### 快照完整性验证 / Snapshot Integrity Verification

```bash
# 验证所有快照
powershell -ExecutionPolicy Bypass -File docs/tools/snapshot-verify.ps1

# 验证特定目录
powershell -ExecutionPolicy Bypass -File docs/tools/snapshot-verify.ps1 -SnapshotDir ./data/snapshots
```

### 快速启动工具 / Quick Start Tools

#### Linux/macOS快速启动 / Linux/macOS Quick Start

```bash
# 运行快速启动脚本
bash scripts/quick-start.sh

# 跳过环境构建
bash scripts/quick-start.sh --skip-build

# 跳过环境启动
bash scripts/quick-start.sh --skip-start
```

#### Windows快速启动 / Windows Quick Start

```powershell
# 运行快速启动脚本
powershell -ExecutionPolicy Bypass -File scripts/quick-start.ps1

# 跳过环境构建
powershell -ExecutionPolicy Bypass -File scripts/quick-start.ps1 -SkipBuild

# 跳过环境启动
powershell -ExecutionPolicy Bypass -File scripts/quick-start.ps1 -SkipStart
```

## 🔬 评测运行 / Evaluation Running

### 综合评测 / Comprehensive Evaluation

运行所有模块的评测：

```bash
# 运行综合评测
bash scripts/run-all-evaluations.sh

# 查看评测结果
ls -la results/
```

### 模块评测 / Module Evaluation

运行特定模块的评测：

```bash
# 知识表示模块评测
bash scripts/kr_eval.sh

# 图论模块评测
bash scripts/gt_eval.sh

# 查看评测结果
cat results/knowledge-representation/evaluation_results.json | python -m json.tool
```

### 评测结果分析 / Evaluation Result Analysis

评测完成后，结果保存在 `results/` 目录中：

```text
results/
├── knowledge-representation/     # 知识表示评测结果
├── graph-theory/                 # 图论评测结果
├── semantic-analysis/            # 语义分析评测结果
├── ontology-engineering/         # 本体工程评测结果
├── knowledge-extraction/         # 知识抽取评测结果
├── reasoning-systems/            # 推理系统评测结果
├── applications/                 # 应用评测结果
├── formal-methods/              # 形式化方法评测结果
├── engineering-practice/         # 工程实践评测结果
├── research-methodology/         # 研究方法论评测结果
└── comprehensive_evaluation_report.json  # 综合评测报告
```

## 📊 项目管理 / Project Management

### 项目状态监控 / Project Status Monitoring

#### 项目完成度检查 / Project Completion Check

```bash
# 运行完成度检查
bash scripts/project-completion-check.sh

# 查看检查结果
echo "项目完成度: $(grep '完成度' results/completion_check.txt)"
```

#### 项目健康检查 / Project Health Check

```bash
# 运行健康检查
bash scripts/health-check.sh

# 查看健康评分
grep "健康评分" results/health_check.txt
```

#### 项目统计报告 / Project Statistics Report

```bash
# 生成统计报告
bash scripts/project-stats.sh

# 查看统计结果
cat results/project_statistics.json | python -m json.tool
```

### 项目维护 / Project Maintenance

#### 项目备份 / Project Backup

```bash
# 创建项目备份
bash scripts/project-manager.sh backup

# 查看备份列表
ls -la backups/
```

#### 项目恢复 / Project Restore

```bash
# 从备份恢复项目
bash scripts/project-manager.sh restore backups/20250101_120000

# 验证恢复结果
bash scripts/project-manager.sh status
```

#### 项目更新 / Project Update

```bash
# 更新项目
bash scripts/project-manager.sh update

# 检查更新结果
git status
```

## 🔧 故障排除 / Troubleshooting

### 常见问题 / Common Issues

#### Docker相关问题 / Docker Related Issues

**问题**: Docker服务未启动

```bash
# 解决方案
sudo systemctl start docker
sudo systemctl enable docker

# 验证Docker状态
docker info
```

**问题**: 权限不足

```bash
# 解决方案
sudo usermod -aG docker $USER
newgrp docker

# 验证权限
docker run hello-world
```

**问题**: 端口冲突

```bash
# 查看端口占用
netstat -tulpn | grep :5000

# 修改Docker Compose配置
# 编辑 env/containers/docker-compose/evaluation.yml
```

#### 脚本执行问题 / Script Execution Issues

**问题**: 脚本权限不足

```bash
# 解决方案
chmod +x scripts/*.sh

# 验证权限
ls -la scripts/
```

**问题**: 脚本语法错误

```bash
# 检查语法
bash -n scripts/script_name.sh

# 修复语法错误
# 使用文本编辑器打开脚本文件
```

**问题**: 路径问题

```bash
# 检查当前目录
pwd

# 确保在项目根目录
cd /path/to/KnowledgeGraph
```

### 日志分析 / Log Analysis

#### Docker日志 / Docker Logs

```bash
# 查看容器日志
docker logs kg-kr-eval

# 实时查看日志
docker logs -f kg-kr-eval

# 查看所有容器状态
docker ps -a
```

#### 脚本日志 / Script Logs

```bash
# 运行脚本并保存日志
bash scripts/script_name.sh > script.log 2>&1

# 查看日志
cat script.log

# 搜索错误信息
grep -i error script.log
```

## 💡 最佳实践 / Best Practices

### 环境管理最佳实践 / Environment Management Best Practices

1. **定期清理**: 定期清理未使用的Docker镜像和容器
2. **版本控制**: 使用标签管理Docker镜像版本
3. **资源监控**: 监控容器的资源使用情况
4. **备份策略**: 定期备份重要的环境配置

### 评测运行最佳实践 / Evaluation Running Best Practices

1. **环境隔离**: 每个评测在独立的环境中运行
2. **结果保存**: 及时保存评测结果和日志
3. **性能监控**: 监控评测过程中的系统性能
4. **错误处理**: 实现完善的错误处理和恢复机制

### 项目管理最佳实践 / Project Management Best Practices

1. **定期检查**: 定期运行项目健康检查和完成度检查
2. **版本管理**: 使用Git管理项目版本
3. **文档更新**: 及时更新项目文档和说明
4. **社区维护**: 建立和维护用户社区

## ❓ 常见问题 / FAQ

### Q: 如何修改Docker环境配置？

A: 编辑对应的Dockerfile文件，然后重新构建镜像：

```bash
docker build -f env/containers/dockerfiles/module_name/Dockerfile \
  -t ghcr.io/kg/module_name-eval:1.0.0 \
  env/containers/dockerfiles/module_name/
```

### Q: 如何添加新的评测模块？

A: 创建新的模块目录、README文件、示例评测报告，并更新项目索引。

### Q: 如何自定义评测指标？

A: 修改评测脚本中的指标计算逻辑，或创建新的评测脚本。

### Q: 如何集成外部数据集？

A: 将数据集放入 `data/snapshots/datasets/` 目录，并生成对应的校验文件。

### Q: 如何扩展项目功能？

A: 在 `scripts/` 目录中添加新的脚本，在 `docs/` 目录中添加相应的文档。

## 📞 获取帮助 / Getting Help

### 官方资源 / Official Resources

- **项目文档**: [docs/](docs/)
- **项目索引**: [docs/KNOWLEDGE_GRAPH_INDEX.md](docs/KNOWLEDGE_GRAPH_INDEX.md)
- **使用标准**: [docs/DOCUMENTATION_STANDARDS.md](docs/DOCUMENTATION_STANDARDS.md)

### 社区支持 / Community Support

- **问题反馈**: 使用GitHub Issues
- **功能建议**: 使用GitHub Discussions
- **合作交流**: 欢迎学术和工业界的合作

### 联系信息 / Contact Information

- **项目维护**: KnowledgeGraph Team
- **技术支持**: 通过GitHub Issues获取支持
- **合作咨询**: 欢迎学术和工业界的合作咨询

---

**最后更新** / Last Updated: 2025-01-01  
**版本** / Version: v1.0.0  
**维护者** / Maintainer: KnowledgeGraph Team  
**许可证** / License: [LICENSE](../../LICENSE)
