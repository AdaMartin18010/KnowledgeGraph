# 统一评测报告模板 / Unified Evaluation Report Template

## 1. 元信息 / Meta Information

- 项目 / Project: [名称 / Name]
- 模块 / Module: [01-10]
- 版本 / Version: [vX.Y.Z]
- 日期 / Date: [YYYY-MM-DD]
- 维护者 / Maintainer: [Name]

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: [描述 / Description]
- 数据集 / Datasets: [名称、版本、来源、快照校验和]
- 指标 / Metrics: [准确性、召回、F1、时延、吞吐、内存等]

## 3. 环境 / Environment

- 硬件 / Hardware: [CPU/GPU/内存/磁盘]
- 软件 / Software: [OS/内核/驱动/CUDA/编译器/框架/库版本]
- 容器 / Container: [镜像标识 / Image Tag]

## 4. 过程 / Procedure

- 配置 / Config: [关键超参、并发、批大小、随机种子]
- 流程 / Pipeline: [训练/推理/评测步骤]
- 脚本 / Scripts: [入口命令/脚本路径]

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Accuracy |   |   |
| Recall |   |   |
| F1 |   |   |
| P50 Latency (ms) |   |   |
| P95 Latency (ms) |   |   |
| Throughput (QPS) |   |   |
| Memory (GB) |   |   |

### 5.2 细分结果 / Detailed Results

- 子集 / Subsets: [域/语言/长度/难度等分组]
- 可解释性 / Explainability: [证据可追溯率、规则命中率]
- 一致性 / Consistency: [跨文档/跨本体/跨表示]

## 6. 对比 / Comparison

- 基线 / Baselines: [名称与链接]
- 提升 / Gains: [相对/绝对提升]

## 7. 结论与建议 / Conclusion & Recommendations

- 结论 / Conclusion: [摘要]
- 风险 / Risks: [局限、边界条件]
- 建议 / Recommendations: [优化方向]

## 8. 复现 / Reproducibility

- 数据快照 / Data Snapshot: [路径、校验和]
- 环境快照 / Environment Snapshot: [清单/镜像]
- 一键运行 / One-click Run: [命令或脚本]

---

**最后更新** / Last Updated: [YYYY-MM-DD]
**版本** / Version: [vX.Y.Z]
