# 知识抽取示例评测报告 / Knowledge Extraction Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 05 Knowledge Extraction
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 实体/关系/事件三任务联合抽取（文本→结构化）
- 数据集 / Datasets: CoNLL03、FewRel、ACE05（快照SHA256: [placeholder]）
- 指标 / Metrics: P/R/F1（实体/关系/事件）、P50/P95、QPS、内存

## 3. 环境 / Environment

- 硬件 / Hardware: 32C CPU, 128GB RAM, 2x A100 40GB
- 软件 / Software: Ubuntu 22.04, CUDA 12.2, Python 3.10
- 容器 / Container: ghcr.io/kg/ke-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: max_len=4096, batch=16, lr=2e-5, seed=42
- 流程 / Pipeline: 预处理 → 训练/微调 → 推理 → 规则/本体一致性校验
- 脚本 / Scripts: scripts/ke_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Entity F1 | 0.924 | CoNLL03 |
| Relation F1 | 0.812 | FewRel |
| Event F1 | 0.705 | ACE05 |
| P50 Latency (ms) | 58 | 单样本 |
| P95 Latency (ms) | 190 | 单样本 |
| Throughput (QPS) | 120 | 8 并发 |
| Memory (GB) | 28.6 | 峰值 |

### 5.2 细分结果 / Detailed Results

- 跨文档聚合：指代与别名合并后关系F1 +1.8%
- 可解释性：证据可追溯率 85%，本体一致性 98.7%

## 6. 对比 / Comparison

- 基线：无规则/本体校验
- 提升：实体F1 +0.6%，关系F1 +2.3%，事件F1 +1.1%

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：本体一致性与跨文档聚合显著提升关系与事件质量
- 风险：长文>4k上下文退化明显
- 建议：RAG召回领域术语，长上下文路由与分块汇总

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{conll03,fewrel,ace05}.sha256
- 环境快照：env/containers/ke-eval-1.0.0.txt
- 一键运行：`bash scripts/ke_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：简易正则实体抽取 / Simple Regex NER

```python
import re
text = "Alice met Bob in Paris on 2024-09-12."
people = re.findall(r"\b[A-Z][a-z]+\b", text)
dates = re.findall(r"\b\d{4}-\d{2}-\d{2}\b", text)
print({"PERSON": people, "DATE": dates})
```

### 9.2 示例表格（表 5-1）/ Example Table (Table 5-1)

| 类别 / Type | 抽取值 / Value |
|-------------|----------------|
| PERSON | Alice, Bob |
| DATE | 2024-09-12 |

> 注 / Note：表 5-1 为示例；生产系统请使用序列标注/指代消解与一致性校验。

## 10. 图与公式编号示例 / Figures & Equations Numbering Examples

- 图 5-1 / Fig 5-1：知识抽取流程示意图 / Knowledge Extraction Pipeline Diagram（占位）
- 公式 (5-1)：F1分数计算 / F1 Score Calculation
  \[F_1 = \frac{2 \times \text{Precision} \times \text{Recall}}{\text{Precision} + \text{Recall}}\]
