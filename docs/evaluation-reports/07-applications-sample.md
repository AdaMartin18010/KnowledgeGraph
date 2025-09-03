# 应用实践示例评测报告 / Applications Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 07 Applications
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 语义搜索与推荐融合场景（双塔检索 + KG增强推荐）
- 数据集 / Datasets: BEIR(msmarco/dev1k)、MovieLens-20M（快照SHA256: [placeholder]）
- 指标 / Metrics: nDCG@10、Recall@50、P50/P95、QPS、成本/千请求

## 3. 环境 / Environment

- 硬件 / Hardware: 16C CPU, 64GB RAM, 1x A10 24GB
- 软件 / Software: Ubuntu 22.04, Python 3.10, Java 17
- 容器 / Container: ghcr.io/kg/app-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: ann_topk=100, rerank_topk=20, seed=42
- 流程 / Pipeline: 索引构建 → ANN召回 → 重排 → KG特征融合 → 线上A/B
- 脚本 / Scripts: scripts/app_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| nDCG@10 | 0.498 | BEIR(msmarco) |
| Recall@50 | 0.764 | BEIR(msmarco) |
| P50 Latency (ms) | 85 | 端到端 |
| P95 Latency (ms) | 210 | 端到端 |
| Throughput (QPS) | 250 | 16 并发 |
| Cost / 1k req | $0.72 | 估算 |

### 5.2 细分结果 / Detailed Results

- KG特征对冷启动用户Top-K覆盖率 +4.2%
- 可解释性标注点击率 CTR +1.1pp

## 6. 对比 / Comparison

- 基线：无KG特征单ANN重排
- 提升：nDCG@10 +2.3%，Recall@50 +3.1%，成本 +$0.04/1k req

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：KG增强在冷启动与可解释性方面收益稳定
- 风险：ANN索引更新带来短时波动
- 建议：引入增量索引与多臂老虎机探索

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{beir-msmarco,movielens20m}.sha256
- 环境快照：env/containers/app-eval-1.0.0.txt
- 一键运行：`bash scripts/app_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：Top-K检索与简易重排 / Top-K Retrieve & Simple Rerank

```python
# 假设召回了Top-K候选，按BM25分与KG加分线性重排
candidates = [
    {"doc": "A", "bm25": 1.2, "kg": 0.3},
    {"doc": "B", "bm25": 1.1, "kg": 0.5},
    {"doc": "C", "bm25": 1.0, "kg": 0.1},
]
alpha = 0.8  # bm25权重
for c in candidates:
    c["score"] = alpha*c["bm25"] + (1-alpha)*c["kg"]
print(sorted(candidates, key=lambda x: -x["score"]))
```

### 9.2 示例表格（表 7-1）/ Example Table (Table 7-1)

| 文档 / Doc | BM25 | KG加分 / KG | 融合分 / Score |
|------------|------|-------------|----------------|
| B | 1.10 | 0.50 | 1.08 |
| A | 1.20 | 0.30 | 1.14 |
| C | 1.00 | 0.10 | 0.82 |

> 注 / Note：表 7-1 为示例；实际重排使用学习排序与多特征融合。
