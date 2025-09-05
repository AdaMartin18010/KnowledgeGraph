---
title: 边缘AI × 知识图谱 实施指南
description: 轻量化知识图谱、边缘检索/推理与云端协同的工程落地路径与参考代码。
---

> 快速总览 / Quick Overview

- **范围**: 轻量存储、边缘检索、边缘-云协同、断连降级与缓存。
- **标准锚点**: RDF/JSON-LD/SHACL 最小集；工程对齐 LDBC/WaTDiv 压测子集。
- **堆栈**: SQLite+轻量 RDF/向量、K3s+Helm、TensorRT/ONNX、Prometheus/Grafana。
- **导航**: 参见 `docs/PROJECT_SUMMARY.md`，与 `docs/12-llm-integration/`、`docs/benchmarks/` 互链。

## 1. 目标与约束

- 低功耗/低内存设备（ARM、Jetson）上的检索与基本推理。
- 轻量图存储（SQLite + RDF 库/嵌入向量本地化）。
- 边缘召回 + 云端生成的混合推理闭环。

## 2. 轻量 KG 与向量检索（Python）

```python
import sqlite3, numpy as np

class EdgeVectorIndex:
    def __init__(self, db_path: str):
        self.conn = sqlite3.connect(db_path)
        self.conn.execute("CREATE TABLE IF NOT EXISTS vec(id TEXT PRIMARY KEY, v BLOB)")

    def add(self, vid: str, vec: np.ndarray):
        self.conn.execute("REPLACE INTO vec(id, v) VALUES(?, ?)", (vid, vec.astype(np.float32).tobytes()))
        self.conn.commit()

    def search(self, q: np.ndarray, top_k: int = 5):
        rows = self.conn.execute("SELECT id, v FROM vec").fetchall()
        sims = []
        for rid, vb in rows:
            v = np.frombuffer(vb, dtype=np.float32)
            sims.append((rid, float(np.dot(q, v) / (np.linalg.norm(q)*np.linalg.norm(v)+1e-9))))
        sims.sort(key=lambda x: x[1], reverse=True)
        return sims[:top_k]
```

## 3. 边缘-云协同协议

- 边缘：本地检索命中实体/片段 → 发送最小证据上下文。
- 云端：拼接上下文 → 大模型生成 → 返回答案与修复建议。
- 断连：边缘降级为模板/规则回答；缓存答复。

## 4. 部署要点

- K3s + Helm 部署轻量服务；NodeFeatureRule 绑定 Jetson 节点。
- INT8/FP16 推理；TensorRT/ONNX Runtime；零拷贝 I/O。
