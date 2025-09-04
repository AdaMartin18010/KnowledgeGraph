# 语义分析示例评测报告 / Semantic Analysis Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 03 Semantic Analysis
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: SRL/关系识别/指代消解与RAG增强评测
- 数据集 / Datasets: CoNLL-2005/2012、DocRED、OntoNotes Coref（快照SHA256: [placeholder]）
- 指标 / Metrics: F1（SRL/Relation）、Coref F1、P50/P95、QPS

## 3. 环境 / Environment

- 硬件 / Hardware: 16C CPU, 64GB RAM, 1x A100 40GB
- 软件 / Software: Ubuntu 22.04, CUDA 12.2, PyTorch 2.2
- 容器 / Container: ghcr.io/kg/sa-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: max_len=3072, batch=16, seed=42
- 流程 / Pipeline: 预处理 → 训练/推理 → RAG召回 → 结构化校验
- 脚本 / Scripts: scripts/sa_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| SRL F1 | 0.867 | CoNLL-2005 |
| Relation F1 | 0.834 | DocRED |
| Coref F1 | 0.717 | OntoNotes |
| P50 Latency (ms) | 64 | 单样本 |
| P95 Latency (ms) | 180 | 单样本 |
| Throughput (QPS) | 135 | 8 并发 |

### 5.2 细分结果 / Detailed Results

- RAG对长尾论元与跨句关系提升更显著：Relation F1 +2.2%

## 6. 对比 / Comparison

- 基线：无RAG与结构化校验
- 提升：SRL F1 +0.9%，Relation F1 +2.2%，Coref F1 +0.8%

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：RAG与结构化验证在跨句任务上收益稳健
- 风险：召回源噪声会放大误配
- 建议：证据去噪与语义可信度阈值

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{conll05,conll12,docred,ontonotes}.sha256
- 环境快照：env/containers/sa-eval-1.0.0.txt
- 一键运行：`bash scripts/sa_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：简易RAG流程 / Simple RAG Pipeline

```python
# 仅示意：BM25检索Top-k句子并拼接到查询前
from rank_bm25 import BM25Okapi

docs = [
  "Paris is the capital of France.",
  "Berlin is the capital of Germany.",
  "The Seine flows through Paris.",
]
tokenized = [d.lower().split() for d in docs]
bm25 = BM25Okapi(tokenized)

query = "Which river runs through the capital of France?"
topk = bm25.get_top_n(query.lower().split(), docs, n=2)
context = " \n".join(topk)
rag_input = f"Context:\n{context}\n\nQuestion:\n{query}\n"
print(rag_input)
```

### 9.2 示例表格（表 3-1）/ Example Table (Table 3-1)

| 项目 / Item | Top-k | 上下文长度 / Context Len |
|-------------|-------|-------------------------|
| 示例 / Example | 2 | 134 |

> 注 / Note：表 3-1 为示例；实际RAG使用向量检索/重排等组件。

## 10. 图与公式编号示例 / Figures & Equations Numbering Examples

- 图 3-1 / Fig 3-1：语义角色标注示例 / Semantic Role Labeling Example（占位）
- 公式 (3-1)：余弦相似度 / Cosine Similarity
  \[\text{sim}(v_1, v_2) = \frac{v_1 \cdot v_2}{\|v_1\| \|v_2\|}\]
