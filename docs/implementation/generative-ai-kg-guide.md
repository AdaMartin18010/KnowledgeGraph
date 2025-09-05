---
title: 生成式AI × 知识图谱 实施指南
description: 知识约束生成、知识注入提示工程与一致性校验的端到端实现模板。
---

> 快速总览 / Quick Overview

- **范围**: 知识约束生成、知识注入、后验一致性校验、幻觉诊断与修复。
- **标准锚点**: RDF/OWL/SHACL、SER/KFR 指标；评测对齐 `benchmarks/ai-kg-fusion-benchmarks.md`。
- **堆栈**: SPARQL 端点、SHACL 引擎、LLM 服务化、向量检索；报告与签名（可选链上存证）。
- **导航**: 参见 `docs/PROJECT_SUMMARY.md` 与 `docs/12-llm-integration/README.md`、`docs/standards/w3c-integration.md`。

## 1. 目标

- 以知识为约束的文本/图像生成；降低幻觉；提升可验证性。

## 2. 提示与约束（示例）

```text
系统：你是领域助手。仅依据提供的三元组作答；不可编造。
用户问题：{query}
三元组证据：
{triples}
输出格式：
- 结论：...
- 证据对齐：对应三元组ID列表
- 不确定性：若证据不足，返回"不足"
```

## 3. 最小代码（Python）

```python
def build_prompt(query: str, triples: list[str]) -> str:
    evidence = "\n".join(triples[:20])
    return f"问题：{query}\n三元组：\n{evidence}\n请严格依据三元组回答，并给出对齐。"

def post_check(answer: str, triples: list[str]) -> dict:
    # 简化一致性核验：关键实体是否在证据中出现
    ents = {t.split()[0] for t in triples}
    ok = any(e in answer for e in ents)
    return {"faithful": ok}
```

## 4. 评测建议

- Faithfulness/Hallucination、Coverage、SER、KFR；人工抽检结合规则校验。

## 5. 部署

- 在线服务化：/generate 接口；灰度与A/B；内容安全审计。
