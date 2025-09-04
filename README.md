# 知识图谱项目 / Knowledge Graph Project

## 🎯 项目概述 / Project Overview

本项目是一个全面的知识图谱技术文档和评测框架，涵盖了从基础理论到工程实践的各个方面，旨在为研究人员和工程师提供标准化的知识图谱解决方案。

## ✨ 主要特性 / Key Features

- **📚 完整文档体系**: 10个核心模块的完整技术文档
- **🔬 标准化评估**: 统一的评测框架和基准数据集
- **🐳 容器化环境**: 11个专业环境的Docker配置
- **🔄 可复现性**: 完整的环境和数据快照管理
- **🌐 双语支持**: 中英文对照，符合国际化标准
- **🛠️ 自动化工具**: 文档检查、快照验证、环境构建脚本

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

#### 1. 构建环境 / Build Environment

```bash
# 构建所有Docker镜像
bash env/containers/scripts/build-all.sh

# 或使用PowerShell (Windows)
powershell -ExecutionPolicy Bypass -File env/containers/scripts/build-all.ps1
```

#### 2. 启动评测环境 / Start Evaluation Environment

```bash
# 使用Docker Compose启动
docker-compose -f env/containers/docker-compose/evaluation.yml up -d
```

#### 3. 进入特定环境 / Enter Specific Environment

```bash
# 知识表示环境
docker exec -it kg-kr-eval bash

# 图论环境
docker exec -it kg-gt-eval bash

# 语义分析环境
docker exec -it kg-sa-eval bash
```

## 📁 项目结构 / Project Structure

```text
KnowledgeGraph/
├── docs/                          # 核心文档
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
│   ├── tools/                        # 工具脚本
│   └── standards/                    # 标准规范
├── data/                          # 数据管理
│   └── snapshots/                 # 数据快照
├── env/                           # 环境配置
│   └── containers/                # 容器环境
├── scripts/                       # 运行脚本
└── README.md                      # 项目说明
```

## 🔧 环境配置 / Environment Configuration

### 容器环境 / Container Environments

| 环境名称 | 标签 | 主要用途 |
|----------|------|----------|
| 知识表示 | `ghcr.io/kg/kr-eval:1.0.0` | 知识表示学习、嵌入训练 |
| 图论 | `ghcr.io/kg/gt-eval:1.0.0` | 图算法、图神经网络 |
| 语义分析 | `ghcr.io/kg/sa-eval:1.0.0` | 语义角色标注、跨语言对齐 |
| 本体工程 | `ghcr.io/kg/oe-eval:1.0.0` | 本体构建、一致性检查 |
| 知识抽取 | `ghcr.io/kg/ke-eval:1.0.0` | 实体关系抽取、事件抽取 |
| 推理系统 | `ghcr.io/kg/rs-eval:1.0.0` | 知识图谱推理、链接预测 |
| 应用 | `ghcr.io/kg/app-eval:1.0.0` | 问答系统、推荐系统 |
| 形式化方法 | `ghcr.io/kg/fm-eval:1.0.0` | 形式化验证、定理证明 |
| 工程实践 | `ghcr.io/kg/ep-eval:1.0.0` | 系统部署、性能评测 |
| 研究方法论 | `ghcr.io/kg/rm-eval:1.0.0` | 实验设计、统计分析 |

### 数据集快照 / Dataset Snapshots

项目支持10个类别的数据集，包括：

- **知识表示**: KILT, LAMA, LUBM
- **图论**: OGBN-Arxiv, OGBN-Products, Cora
- **语义分析**: CoNLL-2012, XNLI, SQuAD
- **本体工程**: DBpedia, Schema.org, WordNet
- **知识抽取**: ACE2005, TAC-KBP, FewRel
- **推理系统**: FB15k-237, WN18RR, YAGO
- **应用**: HotpotQA, MovieLens, BEIR
- **形式化方法**: 自定义规范、标准测试用例
- **工程实践**: LDBC, WatDiv, BSBM
- **研究方法论**: 实验数据、统计测试

## 📖 使用指南 / Usage Guide

### 1. 查看文档 / View Documentation

- **项目索引**: [docs/KNOWLEDGE_GRAPH_INDEX.md](docs/KNOWLEDGE_GRAPH_INDEX.md)
- **评估模板**: [docs/evaluation-report-template.md](docs/evaluation-report-template.md)
- **示例报告**: [docs/evaluation-reports/](docs/evaluation-reports/)

### 2. 运行评测 / Run Evaluation

```bash
# 进入特定环境
docker exec -it kg-kr-eval bash

# 运行评测脚本
bash scripts/kr_eval.sh
```

### 3. 文档检查 / Document Check

```bash
# 检查文档一致性
powershell -ExecutionPolicy Bypass -File docs/tools/docs-check.ps1

# 验证快照完整性
powershell -ExecutionPolicy Bypass -File docs/tools/snapshot-verify.ps1 -SnapshotDir ./data/snapshots
```

## 🛠️ 开发工具 / Development Tools

### 自动化脚本 / Automation Scripts

- **`scripts/quick-start.sh`**: Linux/macOS快速启动脚本
- **`scripts/quick-start.ps1`**: Windows快速启动脚本
- **`env/containers/scripts/build-all.sh`**: 环境构建脚本
- **`env/containers/scripts/build-all.ps1`**: Windows环境构建脚本

### 质量保证工具 / Quality Assurance Tools

- **`docs/tools/docs-check.ps1`**: 文档一致性检查
- **`docs/tools/snapshot-verify.ps1`**: 快照完整性验证

## 📊 评估框架 / Evaluation Framework

### 标准指标 / Standard Metrics

- **准确性**: Precision, Recall, F1-Score
- **效率**: 响应时间、吞吐量、资源利用率
- **可扩展性**: 数据规模、系统性能
- **可复现性**: 环境一致性、结果可验证性

### 基准数据集 / Benchmark Datasets

每个模块都包含标准化的基准数据集和评估协议，确保评测结果的可比性和可复现性。

## 🤝 贡献指南 / Contributing

### 贡献方式 / Ways to Contribute

1. **文档改进**: 完善技术文档和示例
2. **环境优化**: 优化Docker配置和依赖管理
3. **工具开发**: 开发新的自动化工具和脚本
4. **基准扩展**: 添加新的评测基准和数据集
5. **问题报告**: 报告bug和改进建议

### 开发流程 / Development Process

1. Fork项目仓库
2. 创建功能分支
3. 提交代码更改
4. 创建Pull Request
5. 代码审查和合并

## 📄 许可证 / License

本项目采用 [LICENSE](LICENSE) 许可证。

## 📞 联系我们 / Contact Us

- **项目维护**: KnowledgeGraph Team
- **问题反馈**: 请使用GitHub Issues
- **功能建议**: 请使用GitHub Discussions

## 🙏 致谢 / Acknowledgments

感谢所有为知识图谱技术发展做出贡献的研究人员和工程师。

---

**项目状态**: 🟢 核心功能已完成 / Core Features Completed  
**最后更新**: 2025-01-01  
**版本**: v1.0.0  
**维护者**: KnowledgeGraph Team
