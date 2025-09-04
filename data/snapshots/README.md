# 数据快照目录 / Data Snapshots Directory

## 概述 / Overview

本目录包含知识图谱项目使用的各种数据集的快照版本，确保实验的可复现性和一致性。

## 目录结构 / Directory Structure

```text
data/snapshots/
├── README.md                    # 本说明文档
├── datasets/                    # 数据集快照
│   ├── knowledge-representation/  # 知识表示相关数据集
│   ├── graph-theory/             # 图论相关数据集
│   ├── semantic-analysis/        # 语义分析相关数据集
│   ├── ontology-engineering/     # 本体工程相关数据集
│   ├── knowledge-extraction/     # 知识抽取相关数据集
│   ├── reasoning-systems/        # 推理系统相关数据集
│   ├── applications/             # 应用相关数据集
│   ├── formal-methods/           # 形式化方法相关数据集
│   ├── engineering-practice/     # 工程实践相关数据集
│   └── research-methodology/     # 研究方法论相关数据集
└── checksums/                   # 校验和文件
    ├── *.sha256                 # SHA256校验文件
    └── verification.log          # 校验日志
```

## 命名规范 / Naming Conventions

### 数据集快照 / Dataset Snapshots

- 格式：`{dataset-name}-{version}-{date}.{format}`
- 示例：`ogbn-arxiv-v1.0.0-20250101.graphml`

### 校验文件 / Checksum Files

- 格式：`{dataset-name}-{version}-{date}.sha256`
- 示例：`ogbn-arxiv-v1.0.0-20250101.sha256`

## 数据集分类 / Dataset Categories

### 1. 知识表示 / Knowledge Representation

- **KILT**: 知识密集型语言任务
- **LAMA**: 语言模型分析
- **LUBM**: Lehigh大学基准

### 2. 图论 / Graph Theory

- **OGBN-Arxiv**: 开放图基准-论文引用网络
- **OGBN-Products**: 开放图基准-产品网络
- **Cora**: 论文分类数据集

### 3. 语义分析 / Semantic Analysis

- **CoNLL-2012**: 共享任务数据
- **XNLI**: 跨语言自然语言推理
- **SQuAD**: 斯坦福问答数据集

### 4. 本体工程 / Ontology Engineering

- **DBpedia**: 维基百科结构化数据
- **Schema.org**: 结构化数据词汇表
- **WordNet**: 英语词汇数据库

### 5. 知识抽取 / Knowledge Extraction

- **ACE2005**: 自动内容抽取
- **TAC-KBP**: 知识库填充
- **FewRel**: 少样本关系抽取

### 6. 推理系统 / Reasoning Systems

- **FB15k-237**: Freebase知识图谱子集
- **WN18RR**: WordNet关系推理
- **YAGO**: 另一个伟大的本体

### 7. 应用 / Applications

- **HotpotQA**: 多跳问答
- **MovieLens**: 电影推荐数据集
- **BEIR**: 信息检索基准

### 8. 形式化方法 / Formal Methods

- **自定义规范**: 项目特定规范
- **标准测试用例**: 验证测试集

### 9. 工程实践 / Engineering Practice

- **LDBC**: 链接数据基准委员会
- **WatDiv**: 多样性感知测试
- **BSBM**: Berlin SPARQL基准

### 10. 研究方法论 / Research Methodology

- **实验数据**: 对比实验数据
- **统计测试**: 显著性检验数据

## 使用说明 / Usage Instructions

### 下载数据集 / Downloading Datasets

```bash
# 使用wget下载数据集
wget -O data/snapshots/datasets/{category}/{dataset-name}.{format} {url}

# 生成校验和
sha256sum data/snapshots/datasets/{category}/{dataset-name}.{format} > \
  data/snapshots/checksums/{dataset-name}.sha256
```

### 验证数据集完整性 / Verifying Dataset Integrity

```bash
# 使用PowerShell脚本验证
powershell -ExecutionPolicy Bypass -File docs/tools/snapshot-verify.ps1 -SnapshotDir ./data/snapshots

# 或手动验证
sha256sum -c data/snapshots/checksums/{dataset-name}.sha256
```

### 更新数据集 / Updating Datasets

1. 下载新版本数据集
2. 生成新的校验和
3. 更新版本号
4. 记录变更日志

## 注意事项 / Important Notes

1. **版本控制**: 所有数据集都应标注版本号和日期
2. **校验和**: 每个数据集必须有对应的SHA256校验文件
3. **文档化**: 数据集的使用方法和预处理步骤应详细记录
4. **许可证**: 确保遵守各数据集的许可证要求
5. **存储**: 大文件建议使用外部存储或压缩存储

## 维护者 / Maintainers

- 项目团队负责数据集的更新和维护
- 定期检查数据集的可用性和完整性
- 及时处理数据集相关的issue和问题

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0
