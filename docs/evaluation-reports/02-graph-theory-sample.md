# 图论基础示例评测报告 / Graph Theory Fundamentals Sample Evaluation Report

## 1. 元信息 / Meta Information

- 项目 / Project: KnowledgeGraph
- 模块 / Module: 02 Graph Theory
- 版本 / Version: v1.0.0
- 日期 / Date: 2025-01-01
- 维护者 / Maintainer: Team

## 2. 评测范围 / Evaluation Scope

- 任务 / Task: 图算法与GNN实现的性能与可扩展性评测
- 数据集 / Datasets: OGBN-Arxiv、OGBN-Products（快照SHA256: [placeholder]）
- 指标 / Metrics: P50/P95、QPS、采样效率、GPU/内存占用、准确率

## 3. 环境 / Environment

- 硬件 / Hardware: 32C CPU, 128GB RAM, 2x A100 40GB
- 软件 / Software: Ubuntu 22.04, CUDA 12.2, PyTorch 2.2, DGL 1.1
- 容器 / Container: ghcr.io/kg/gt-eval:1.0.0

## 4. 过程 / Procedure

- 配置 / Config: sampler=graphsage, fanout=15,15,20, batch=1024
- 流程 / Pipeline: 预处理 → 采样/训练 → 推理 → 指标汇总
- 脚本 / Scripts: scripts/gt_eval.sh

## 5. 结果 / Results

### 5.1 指标汇总 / Metrics Summary

| 指标 / Metric | 值 / Value | 备注 / Notes |
|---------------|-----------|--------------|
| Train Acc | 0.924 | OGBN-Arxiv |
| Eval Acc | 0.901 | OGBN-Arxiv |
| P50 Latency (ms) | 35 | 推理批 |
| P95 Latency (ms) | 110 | 推理批 |
| Throughput (QPS) | 220 | 2xA100 |
| GPU Memory (GB) | 31.4 | 峰值 |

### 5.2 细分结果 / Detailed Results

- 动态采样相较全图推理，P95 -18%，Eval Acc -0.4pp（可接受）

## 6. 对比 / Comparison

- 基线：全图卷积推理（无采样）
- 提升：QPS +28%，P95 -24ms

## 7. 结论与建议 / Conclusion & Recommendations

- 结论：层次化采样在大图上高效稳健
- 风险：节点度分布极端时方差增大
- 建议：引入度感知采样与混合缓存

## 8. 复现 / Reproducibility

- 数据快照：data/snapshots/{ogbn-arxiv,ogbn-products}.sha256
- 环境快照：env/containers/gt-eval-1.0.0.txt
- 一键运行：`bash scripts/gt_eval.sh`

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0

## 9. 附录：最小可运行示例 / Appendix: Minimal Runnable Example

### 9.1 Python：Dijkstra最短路 / Dijkstra Shortest Path

```python
import heapq
from collections import defaultdict

def dijkstra(edges, start):
    g = defaultdict(list)
    for u, v, w in edges:
        g[u].append((v, w))
        g[v].append((u, w))
    dist = defaultdict(lambda: float('inf'))
    dist[start] = 0
    pq = [(0, start)]
    while pq:
        d, u = heapq.heappop(pq)
        if d > dist[u]:
            continue
        for v, w in g[u]:
            nd = d + w
            if nd < dist[v]:
                dist[v] = nd
                heapq.heappush(pq, (nd, v))
    return dict(dist)

edges = [("A","B",1),("B","C",2),("A","C",5)]
print(dijkstra(edges, "A"))
```

### 9.2 示例表格（表 2-1）/ Example Table (Table 2-1)

| 源 / Src | 目的 / Dst | 路径代价 / Cost |
|----------|------------|-----------------|
| A | B | 1 |
| A | C | 3 |

> 注 / Note：表 2-1 为示例；真实实验以框架结果为准。
