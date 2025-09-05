# 文档规范与治理 / Documentation Standards & Governance

> 快速总览 / Quick Overview

- **范围**: 文档编号/命名/链接/版本/校验/引用/治理的统一规范与CI门禁。
- **标准锚点**: 与 `docs/PROJECT_SUMMARY.md` 与 `docs/benchmarks/` 报告规范对齐；外链优先 W3C/ISO/顶级课程/论文。
- **堆栈**: Markdown Lint、链接校验、`tools/snapshot-verify.ps1`、SHACL 形状与术语对齐校验、CI Gate。
- **导航**: 参见 `docs/PROJECT_SUMMARY.md`，与 `docs/integration/UNIFIED-FUSION-FRAMEWORK.md`、`docs/standards/w3c-integration.md`、各主题 README 顶部“快速总览”互链。

> 版本: v0.1 | 维护: KnowledgeGraph Team | 适用范围: docs/*

## 1. 统一编号与标题

- 章节-小节-条款三级编号（如 4.2.1），中英双语标题
- 每个文档顶部包含“规范化区块（元数据）”：版本、状态、上游/外部映射

## 2. 命名与链接策略

- 术语/概念以 IRI/前缀（schema:/wd:/dbo:/ex:）为主键
- 内部链接：同层互链、上下游链（PROJECT_SUMMARY 索引）
- 外部链接：W3C/Wikipedia/Wikidata/标准文档，使用稳定URL

## 3. 去重与合并

- 不删除批判性原文；新增“规范化区块”对齐与指正
- 以术语为主键进行归并，保留来源与差异说明

## 4. 版本与快照

- 重要里程碑产出“快照报告”，包含链接检查、孤儿页、断链统计
- 使用 `tools/snapshot-verify.ps1` 生成清单

## 5. 校验与CI

- Markdown Lint：结尾换行、标题级别、链接完整性
- 语义校验：SHACL（如适用）、术语表对齐（terminology-dictionary）
- PR Gate：规范区块存在性、必需外链有效性、断链阈值

## 6. 引用与学术规范

- 优先引用权威条目（W3C/ISO/百科/课程主页/论文）
- 标注访问日期；为PDF/预印本提供稳定镜像链接

## 7. 文档结构建议

- 开头：元数据区块 + 导航
- 正文：分层、对标、方法、工程、评测、治理
- 结尾：参考与交叉引用、更新信息

## 8. 变更治理

- 每次修改记录“变更要点”“影响范围”“回滚方式”
- 对跨文档影响的更改创建追踪 Issue 并在相关文档交叉链接
