# 知识表示示例评测报告 / Knowledge Representation Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 01 Knowledge Representation
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 表征正确性/一致性/完备性与语义保持性评测
- 数据集 / Datasets: KILT、LAMA、LUBM（快照SHA256: [placeholder]）
- 指标 / Metrics: Correctness、Consistency Violations、Coverage、Semantic Equivalence Rate

## 3. 环境 / Environment

- 硬件 / Hardware: 8C CPU, 32GB RAM
- 软件 / Software: Ubuntu 22.04, Python 3.10, Jena, OWL API
- 容器 / Container: ghcr.io/kg/kr-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: mapping_rules=v1, seed=42
- 流程 / Pipeline: 模型构建 → 映射/转换 → 语义等价检查 → 一致性与覆盖评测
- 脚本 / Scripts: scripts/kr_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Correctness | 98.3% | 黄金集 |
| Consistency Violations | 5 | / 2k 规则 |
| Coverage | 93.1% | 关键概念/关系 |
| Semantic Equivalence Rate | 97.4% | 映射后等价率 |

### 5.2 细分结果 / Detailed Results

- 高义项多义词处等价率下降；补充词汇表后恢复 +1.2pp

## 6. 对比 / Comparison

- 基线：旧版映射规则 v0
- 提升：等价率 +2.0pp，覆盖 +1.6pp，矛盾 -3

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：标准化映射与词汇增强显著提升等价与覆盖
- 风险：跨域本体不一致时矛盾上升
- 建议：引入对齐先验与版本化映射

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{kilt,lama,lubm}.sha256
- 环境快照：env/containers/kr-eval-1.0.0.txt
- 一键运行：`bash scripts/kr_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：语义等价近似（Jaccard） / Approximate Semantic Equivalence

```python
# 粗略示例：用Jaccard评估两段定义的一致性（词袋近似）
def jaccard(a: str, b: str) -> float:
    sa = set(a.lower().split())
    sb = set(b.lower().split())
    inter = len(sa & sb)
    union = len(sa | sb) or 1
    return inter / union

ref = "entity relation attribute instance mapping"
cand = "entity mapping relation instance property"
print(f"Jaccard ≈ {jaccard(ref, cand):.3f}")
```

### 9.2 示例表格（表 1-1）/ Example Table (Table 1-1)

| 项目 / Item | 参考定义 / Reference | 候选定义 / Candidate | Jaccard |
|-------------|----------------------|----------------------|---------|
| 示例 / Example | entity relation attribute instance mapping | entity mapping relation instance property | 0.667 |

> 注 / Note：表 1-1 为示例，实际语义保持性以正式评测为准。
