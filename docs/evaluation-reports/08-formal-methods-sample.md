# 形式化方法示例评测报告 / Formal Methods Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 08 Formal Methods
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 模型检查与SMT求解正确性与效率评测
- 数据集 / Datasets: SV-COMP子集、SMT-COMP QF_LIA/QF_BV（快照SHA256: [placeholder]）
- 指标 / Metrics: Correctness、Timeout/Memory-out、Runtime、Peak Memory

## 3. 环境 / Environment

- 硬件 / Hardware: 16C CPU, 64GB RAM
- 软件 / Software: Ubuntu 22.04, Z3 4.12, CVC5 1.1, NuSMV 2.6
- 容器 / Container: ghcr.io/kg/fm-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: timeout=300s, mem=16GB, seed=42
- 流程 / Pipeline: 基准选择 → 工具运行 → 证据/反例收集 → 报告生成
- 脚本 / Scripts: scripts/fm_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Correct Cases | 312 | / 330 |
| Timeouts | 11 | 300s |
| Memory-outs | 2 | 16GB |
| Median Runtime (s) | 1.84 |   |
| Peak Memory (GB) | 7.6 |   |

### 5.2 细分结果 / Detailed Results

- QF_BV 对复杂位运算实例更敏感；适度预简化可降 12% 运行时
- 反例可视化输出覆盖率 93%

## 6. 对比 / Comparison

- 基线：单一求解器默认配置
- 提升：多求解器投票 + 预处理，Correct Cases +2.7%，Median -0.21s

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：多工具组合与预处理可在保持正确性的同时提升效率
- 风险：参数敏感与版本差异引入波动
- 建议：固化容器镜像与参数模板，增加回归集

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{svcomp, smtcomp}.sha256
- 环境快照：env/containers/fm-eval-1.0.0.txt
- 一键运行：`bash scripts/fm_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：Z3线性不等式求解 / Z3 Linear Inequalities

```python
from z3 import Ints, Solver, And
x, y = Ints('x y')
solver = Solver()
solver.add(And(x + y > 10, x >= 0, y >= 0))
print(solver.check())
print(solver.model())
```

### 9.2 示例表格（表 8-1）/ Example Table (Table 8-1)

| 变量 / Var | 值 / Value |
|------------|-----------|
| x | 6 |
| y | 5 |

> 注 / Note：表 8-1 为示例；实际评测需记录反例与证明证据。
