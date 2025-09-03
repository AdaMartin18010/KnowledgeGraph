# 推理系统示例评测报告 / Reasoning Systems Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 06 Reasoning Systems
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 规则推理 + 检索增强推理（RAG）一致性与效率评测
- 数据集 / Datasets: LUBM(1K)、FB15k-237（快照SHA256: [placeholder]）
- 指标 / Metrics: Precision、Recall、F1、P50/P95 Latency、Throughput、Memory

## 3. 环境 / Environment

- 硬件 / Hardware: 16C CPU, 64GB RAM, 1x A100 40GB
- 软件 / Software: Ubuntu 22.04, CUDA 12.2, Python 3.10, Java 17
- 容器 / Container: ghcr.io/kg/rs-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: batch=32, max_depth=3, seed=42
- 流程 / Pipeline: 索引构建 → 前向推理 → 工具化检索 → 规则校验 → 汇总
- 脚本 / Scripts: scripts/rs_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Precision | 0.89 |   |
| Recall | 0.86 |   |
| F1 | 0.875 |   |
| P50 Latency (ms) | 42 | 单查询 |
| P95 Latency (ms) | 120 | 单查询 |
| Throughput (QPS) | 180 | 8 并发 |
| Memory (GB) | 14.2 | 峰值 |

### 5.2 细分结果 / Detailed Results

- 子集 / Subsets: LUBM 推理深度2/3 分组；FB15k-237 关系稀有度分组
- 可解释性 / Explainability: 规则命中率 78%，推理路径可视化率 92%
- 一致性 / Consistency: 与本体约束一致率 99.2%

## 6. 对比 / Comparison

- 基线 / Baselines: 仅规则推理（无检索）
- 提升 / Gains: F1 +2.1%，P95 时延 +8ms（可接受）

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：工具化检索与规则推理组合在可解释性与一致性上收益明显
- 风险：检索索引更新频率影响一致性；深度>3 时延上升
- 建议：加入动态缓存与索引版本化；规则蒸馏进一步降时延

## 8. 复现 / Reproducibility

- 数据快照 / Data Snapshot: data/snapshots/{lubm,fb15k237}.sha256
- 环境快照 / Environment Snapshot: env/containers/rs-eval-1.0.0.txt
- 一键运行 / One-click Run: `bash scripts/rs_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：前向链式极简规则引擎 / Minimal Forward-chaining

```python
facts = {("parent", "alice", "bob"), ("parent", "bob", "carol")}
rules = [
  # parent(X,Y) & parent(Y,Z) -> grandparent(X,Z)
  ([("parent", "X", "Y"), ("parent", "Y", "Z")], ("grandparent", "X", "Z"))
]

def match(pattern, fact):
  pred, a, b = pattern
  fpred, fa, fb = fact
  if pred != fpred:
    return None
  env = {}
  def bind(var, val):
    if var.isupper():
      if var in env and env[var] != val: return False
      env[var] = val
    else:
      if var != val: return False
    return True
  if not bind(a, fa): return None
  if not bind(b, fb): return None
  return env

changed = True
while changed:
  changed = False
  for antecedents, consequent in rules:
    # 枚举匹配
    envs = [match(antecedents[0], f) for f in list(facts)]
    for e1 in filter(None, envs):
      for f2 in list(facts):
        e2 = match(antecedents[1], f2)
        if not e2: continue
        env = {**e1, **e2}
        pred, x, z = consequent
        new_fact = (pred, env[x], env[z])
        if new_fact not in facts:
          facts.add(new_fact)
          changed = True

print(sorted(facts))
```

### 9.2 示例表格（表 6-1）/ Example Table (Table 6-1)

| 谓词 / Predicate | 主体 / Subj | 客体 / Obj |
|------------------|-------------|------------|
| parent | alice | bob |
| parent | bob | carol |
| grandparent | alice | carol |

> 注 / Note：表 6-1 为示例；生产推理需采用高效索引与冲突集策略。
