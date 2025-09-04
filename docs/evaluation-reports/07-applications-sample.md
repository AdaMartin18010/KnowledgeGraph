# 知识图谱应用示例评测报告 / Knowledge Graph Applications Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 07 Applications
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 问答系统与推荐系统的效果与用户体验评测
- 数据集 / Datasets: HotpotQA、MovieLens（快照SHA256: [placeholder]）
- 指标 / Metrics: 准确率、用户满意度、响应时间、系统可用性

## 3. 环境 / Environment

- 硬件 / Hardware: 16C CPU, 64GB RAM, 2x V100 32GB
- 软件 / Software: Ubuntu 20.04, Python 3.9, Flask 2.3
- 容器 / Container: ghcr.io/kg/app-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: model=bert-large, max_qa_length=512
- 流程 / Pipeline: 用户输入 → 知识检索 → 推理生成 → 结果展示
- 脚本 / Scripts: scripts/app_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| QA准确率 | 0.734 | HotpotQA |
| 推荐准确率 | 0.823 | MovieLens |
| 平均响应时间 (s) | 1.2 | 端到端 |
| 用户满意度 | 4.2/5.0 | 问卷调查 |
| 系统可用性 | 99.7% | 7x24监控 |

### 5.2 细分结果 / Detailed Results

- 问题类型: 事实型(0.81), 推理型(0.67), 比较型(0.58)

## 6. 对比 / Comparison

- 基线：传统检索系统
- 提升：QA准确率 +0.28，推荐准确率 +0.31

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：知识图谱显著提升应用系统性能
- 风险：复杂查询处理时间增加
- 建议：引入查询优化与结果缓存机制

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{hotpotqa,movielens}.sha256
- 环境快照：env/containers/app-eval-1.0.0.txt
- 一键运行：`bash scripts/app_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：简单问答系统 / Simple QA System

```python
from typing import Dict, List, Optional

class SimpleQASystem:
    def __init__(self):
        self.knowledge_base = {
            "北京": {"首都": "中国", "人口": "2154万"},
            "上海": {"首都": "中国", "人口": "2428万"},
            "中国": {"首都": "北京", "人口": "14亿"}
        }
    
    def answer_question(self, question: str) -> Optional[str]:
        question = question.lower()
        
        if "首都" in question:
            for entity, info in self.knowledge_base.items():
                if entity in question:
                    return f"{entity}的首都是{info['首都']}"
        
        elif "人口" in question:
            for entity, info in self.knowledge_base.items():
                if entity in question:
                    return f"{entity}的人口是{info['人口']}"
        
        return "抱歉，我无法回答这个问题。"

# 示例使用
qa_system = SimpleQASystem()
print(qa_system.answer_question("北京的首都是什么？"))
print(qa_system.answer_question("上海的人口是多少？"))
```

### 9.2 示例表格（表 7-1）/ Example Table (Table 7-1)

| 应用类型 / Application Type | 准确率 / Accuracy | 响应时间 / Response Time | 用户满意度 / User Satisfaction |
|------------------------------|-------------------|---------------------------|----------------------------------|
| 问答系统 / QA System | 0.73 | 1.2s | 4.2/5.0 |
| 推荐系统 / Recommendation | 0.82 | 0.8s | 4.5/5.0 |

> 注 / Note：表 7-1 为示例；真实实验以框架结果为准。

## 10. 图与公式编号示例 / Figures & Equations Numbering Examples

- 图 7-1 / Fig 7-1：应用系统架构图 / Application System Architecture Diagram（占位）
- 公式 (7-1)：用户满意度计算 / User Satisfaction Calculation
  \[\text{Satisfaction} = \frac{1}{N} \sum_{i=1}^{N} \text{rating}_i\]
