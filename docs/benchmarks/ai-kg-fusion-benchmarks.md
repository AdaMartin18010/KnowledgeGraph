---
title: AI × KG 融合评测基准
description: 定义跨场景（RAG、多模态、RL、FL、生成式、边缘/云）的一致评测指标、数据集映射、协议与可复现实验管线。
---

> 快速总览 / Quick Overview

- **标准锚点**: W3C（RDF/OWL/SPARQL/SHACL）× ML 基准（KILT/OGB/BEIR）× 工程（LDBC/WaTDiv/BSBM）。
- **评测范围**: RAG、KG 推理、多模态、检索/生成一致性、工程性能与成本。
- **堆栈参考**: SPARQL 端点、向量检索（FAISS/PGVector）、评测脚本与CI；形状校验与一致性报告。
- **导航**: 参见 `docs/PROJECT_SUMMARY.md` 快速总览，与 `docs/12-llm-integration/`、`docs/06-reasoning-systems/`、`docs/standards/w3c-integration.md` 互链。

> 规范化区块（元数据）
> 统一编号映射: 6 工程评测 / 7 标准与生态
> 上游索引: `docs/PROJECT_SUMMARY.md` → 6/7；映射：`docs/integration/UNIFIED-FUSION-FRAMEWORK.md` 第7节；外部基准：LDBC/WaTDiv/BSBM/KILT/OGB。

## 1. 评测维度

- 正确性：Precision / Recall / F1、Semantic Equivalence、Consistency Violations。
- 性能：Latency（P50/P95/P99）、Throughput、GPU/Memory/Cost。
- 鲁棒性：领域迁移、长上下文、对抗噪声。
- 可信性：可追溯性、可解释性、隐私泄漏率、偏见与公平性。
- 体验与ROI：用户满意度、任务完成率、单位成本收益。

## 2. 数据集映射（示例）

- RAG/问答：KILT、BEIR、HotpotQA、TQA；KG 链接：DBpedia、YAGO。
- KG 任务：FB15k-237、WN18RR、LUBM、OGBN-Arxiv/Products、LDBC、WatDiv、BSBM。
- 信息抽取：CoNLL、ACE2005、DocRED、TAC-KBP、FewRel、OntoNotes Coref。
- 多模态：MSCOCO、Flickr30k、WebVid、LAION-400M（需合规采样）。
- 推荐/图挖掘：MovieLens、OGB 系列。

### 2.1 最小可复现清单（V1）

- 语义网推理/查询：LUBM(1/5/50)、WatDiv SF10、BSBM 10M
- 知识推理：FB15k-237、WN18RR（链接预测/三元组补全）
- RAG 问答：KILT: NQ/TQA；BEIR: FiQA/NFCorpus
- 多模态 QA：COCOQA 子集（仅演示用途）
- 统一脚本：`tools/bench/run_kg_reason.py`、`run_rag.py`、`run_mmqa.py`

## 3. 指标定义与计算

- SER（Semantic Equivalence Rate）：基于语义匹配与知识校验的等价率。
- CV（Consistency Violations）：基于 OWL 约束与规则的违反条数/率。
- HR（Hallucination Rate）：基于可验证证据的虚构比例。
- KCR（Knowledge Coverage Rate）：命中与覆盖的比例（基于三元组/实体）。
- KFR（Knowledge Faithfulness Rate）：生成内容可由证据与规则推导的比例。

## 4. 评测协议（Pipeline）

1) 数据准备：下载→清洗→切分（train/dev/test）→版本化（DVC/MLflow）。
2) 检索面：嵌入构建→索引→召回/重排→指标计算（Recall@k、MRR、nDCG）。
3) 生成面：上下文构建→LLM 推断→语义/一致性校验→人工抽检。
4) 多模态：文本/图像编码→对齐→跨模态检索/问答→指标汇总。
5) 工程面：压测→资源统计→成本核算→弹性与回滚验证。

## 5. 复现实验（参考脚本）

```bash
python tools/bench/run_rag.py --dataset kilt --model qwen2-instruct --index faiss --top_k 10
python tools/bench/run_mmqa.py --dataset cocoqa --clip openai/clip-vit-base-patch32
python tools/bench/run_kg_reason.py --dataset fb15k237 --rule owl
python tools/bench/run_sparql_bench.py --suite lubm --endpoint http://localhost:3030/ds --queries tools/bench/queries/lubm
python tools/bench/run_ldbc.py --scale 1 --workload interactive
```

## 6. 报告模板（最小集）

- 环境：硬件/软件/模型版本、随机种子、数据版本。
- 结果：核心指标表格 + 误差条 + 置信区间（如适用）。
- 对比：SOTA/开源实现/自家基线。
- 诊断：失败案例簇、误差来源、偏倚分析。
- 结论：可行性、局限、改进计划与影响评估。

## 7. 发布与签名

- 导出结果 JSON + 图表；生成评测卡（Benchmark Card）。
- 使用链上签名（可选）对知识版本与结果进行可验证存证。
