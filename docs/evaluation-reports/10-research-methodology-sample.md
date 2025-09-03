# 研究方法论示例评测报告 / Research Methodology Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 10 Research Methodology
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 研究复现性/显著性/透明度评测
- 数据集 / Datasets: HotpotQA、OGB、LUBM（快照SHA256: [placeholder]）
- 指标 / Metrics: Reproducibility Rate、CI/Std、p-value、Transparency Score

## 3. 环境 / Environment

- 硬件 / Hardware: 8C CPU, 32GB RAM
- 软件 / Software: Ubuntu 22.04, Python 3.10, R 4.3
- 容器 / Container: ghcr.io/kg/rm-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: seeds=[1..10], alpha=0.05
- 流程 / Pipeline: 数据快照 → 实验重跑×10 → 统计检验 → 报告
- 脚本 / Scripts: scripts/rm_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Reproducibility Rate | 0.92 | 10次重跑 |
| CI@95% (F1) | [0.842, 0.856] |   |
| p-value | 0.018 | 对比基线 |
| Transparency Score | 0.88 | 文档/脚本/环境公开度 |

### 5.2 细分结果 / Detailed Results

- 部分外部依赖版本浮动导致方差升高；容器固化后改善

## 6. 对比 / Comparison

- 基线：单次实验报告、无种子控制
- 提升：复现成功率 +0.31，CI 收窄 24%

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：多次重跑+容器固化显著提升复现与可信度
- 风险：外部服务依赖不可控
- 建议：本地化镜像仓库与结果签名

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{hotpotqa,ogb,lubm}.sha256
- 环境快照：env/containers/rm-eval-1.0.0.txt
- 一键运行：`bash scripts/rm_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Examples

### 9.1 Python：均值/置信区间与独立样本t检验 / Mean, CI, t-test

```python
import numpy as np
from scipy import stats

# 两组F1分数（示例数据）
A = np.array([0.85, 0.84, 0.86, 0.85, 0.853, 0.847, 0.849, 0.852, 0.851, 0.848])
B = np.array([0.842, 0.839, 0.845, 0.838, 0.841, 0.836, 0.844, 0.837, 0.840, 0.839])

alpha = 0.05
# 组A均值与95%CI
mean_A = A.mean()
sem_A = stats.sem(A)
ci_A = stats.t.interval(1 - alpha, len(A) - 1, loc=mean_A, scale=sem_A)

# 独立样本双尾t检验（等方差可按Levene检验判定；此处示例equal_var=False）
t_stat, p_value = stats.ttest_ind(A, B, equal_var=False)

print(f"A mean={mean_A:.3f}, 95%CI={ci_A}")
print(f"t={t_stat:.3f}, p={p_value:.3g}")
```

### 9.2 R：均值/置信区间与t检验 / Mean, CI, t-test

```r
# 示例数据
A <- c(0.85,0.84,0.86,0.85,0.853,0.847,0.849,0.852,0.851,0.848)
B <- c(0.842,0.839,0.845,0.838,0.841,0.836,0.844,0.837,0.840,0.839)

alpha <- 0.05
# 组A均值和置信区间（t分布）
mean_A <- mean(A)
se_A <- sd(A)/sqrt(length(A))
tcrit <- qt(1 - alpha/2, df=length(A)-1)
ci_A <- c(mean_A - tcrit*se_A, mean_A + tcrit*se_A)

# Welch t检验（不等方差）
res <- t.test(A, B, var.equal=FALSE)
print(list(mean_A=mean_A, CI_A=ci_A, t=res$statistic, p=res$p.value))
```

### 9.3 示例表格（表 10-1）/ Example Table (Table 10-1)

| 项目 / Item | 组A均值 / Group A Mean | 组A 95%CI / Group A 95%CI | t值 / t | p值 / p |
|-------------|------------------------|---------------------------|---------|---------|
| 示例 / Example | 0.850 | [0.845, 0.855] | 2.37 | 0.018 |

> 注 / Note：表 10-1 为示例结果，实际数值以脚本输出为准。
