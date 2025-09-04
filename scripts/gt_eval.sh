#!/bin/bash

# 图论模块评测脚本 / Graph Theory Evaluation Script
# 作者 / Author: KnowledgeGraph Team
# 版本 / Version: 1.0.0

set -e

# 颜色定义 / Color Definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 日志函数 / Logging Functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 检查环境 / Check Environment
check_environment() {
    log_info "检查图论评测环境 / Checking Graph Theory evaluation environment..."
    
    # 检查Python环境 / Check Python environment
    if ! command -v python &> /dev/null; then
        log_error "Python未安装或不在PATH中"
        exit 1
    fi
    
    # 检查必要的Python包 / Check required Python packages
    python -c "
import torch
import dgl
import networkx
import matplotlib
import numpy
print('所有必要的包都已安装 / All required packages are installed')
" 2>/dev/null || {
        log_error "缺少必要的Python包，请安装依赖"
        exit 1
    }
    
    log_success "环境检查通过 / Environment check passed"
}

# 运行图论评测 / Run Graph Theory Evaluation
run_evaluation() {
    log_info "开始图论评测 / Starting Graph Theory evaluation..."
    
    # 创建结果目录 / Create results directory
    mkdir -p results/graph-theory
    
    # 运行评测脚本 / Run evaluation script
    python << 'EOF'
import torch
import dgl
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import time
import json
from datetime import datetime

print("=== 图论模块评测 / Graph Theory Module Evaluation ===")
print(f"评测时间 / Evaluation Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
print()

# 1. 环境信息 / Environment Information
print("1. 环境信息 / Environment Information")
print(f"   Python版本 / Python Version: {torch.version.__version__}")
print(f"   PyTorch版本 / PyTorch Version: {torch.__version__}")
print(f"   DGL版本 / DGL Version: {dgl.__version__}")
print(f"   NetworkX版本 / NetworkX Version: {nx.__version__}")
print(f"   CUDA可用 / CUDA Available: {torch.cuda.is_available()}")
if torch.cuda.is_available():
    print(f"   CUDA版本 / CUDA Version: {torch.version.cuda}")
    print(f"   GPU数量 / GPU Count: {torch.cuda.device_count()}")
print()

# 2. 图结构创建测试 / Graph Structure Creation Test
print("2. 图结构创建测试 / Graph Structure Creation Test")
start_time = time.time()
try:
    # 创建NetworkX图 / Create NetworkX graph
    G_nx = nx.erdos_renyi_graph(100, 0.1)
    nx_time = time.time() - start_time
    print(f"   ✅ NetworkX图创建成功 / NetworkX graph created successfully")
    print(f"   节点数量 / Node count: {G_nx.number_of_nodes()}")
    print(f"   边数量 / Edge count: {G_nx.number_of_edges()}")
    print(f"   创建时间 / Creation time: {nx_time:.4f}s")
except Exception as e:
    print(f"   ❌ NetworkX图创建失败 / NetworkX graph creation failed: {e}")
print()

# 3. DGL图创建测试 / DGL Graph Creation Test
print("3. DGL图创建测试 / DGL Graph Creation Test")
start_time = time.time()
try:
    # 创建DGL图 / Create DGL graph
    src = torch.randint(0, 100, (200,))
    dst = torch.randint(0, 100, (200,))
    G_dgl = dgl.graph((src, dst), num_nodes=100)
    dgl_time = time.time() - start_time
    print(f"   ✅ DGL图创建成功 / DGL graph created successfully")
    print(f"   节点数量 / Node count: {G_dgl.number_of_nodes()}")
    print(f"   边数量 / Edge count: {G_dgl.number_of_edges()}")
    print(f"   创建时间 / Creation time: {dgl_time:.4f}s")
except Exception as e:
    print(f"   ❌ DGL图创建失败 / DGL graph creation failed: {e}")
print()

# 4. 图算法测试 / Graph Algorithm Test
print("4. 图算法测试 / Graph Algorithm Test")
try:
    # 最短路径算法 / Shortest path algorithm
    start_time = time.time()
    path = nx.shortest_path(G_nx, 0, 50)
    sp_time = time.time() - start_time
    print(f"   ✅ 最短路径算法成功 / Shortest path algorithm successful")
    print(f"   路径长度 / Path length: {len(path)}")
    print(f"   计算时间 / Computation time: {sp_time:.4f}s")
    
    # 连通分量算法 / Connected components algorithm
    start_time = time.time()
    components = list(nx.connected_components(G_nx))
    cc_time = time.time() - start_time
    print(f"   ✅ 连通分量算法成功 / Connected components algorithm successful")
    print(f"   连通分量数量 / Component count: {len(components)}")
    print(f"   计算时间 / Computation time: {cc_time:.4f}s")
    
    # 中心性算法 / Centrality algorithm
    start_time = time.time()
    centrality = nx.betweenness_centrality(G_nx)
    bc_time = time.time() - start_time
    print(f"   ✅ 中心性算法成功 / Centrality algorithm successful")
    print(f"   计算时间 / Computation time: {bc_time:.4f}s")
    
except Exception as e:
    print(f"   ❌ 图算法测试失败 / Graph algorithm test failed: {e}")
print()

# 5. 图神经网络测试 / Graph Neural Network Test
print("5. 图神经网络测试 / Graph Neural Network Test")
try:
    # 创建节点特征 / Create node features
    node_features = torch.randn(100, 16)
    G_dgl.ndata['feat'] = node_features
    
    # 简单的图卷积层 / Simple graph convolution layer
    class SimpleGCN(torch.nn.Module):
        def __init__(self, in_dim, out_dim):
            super(SimpleGCN, self).__init__()
            self.linear = torch.nn.Linear(in_dim, out_dim)
        
        def forward(self, g, features):
            g.ndata['h'] = features
            g.update_all(dgl.function.copy_u('h', 'm'), dgl.function.sum('m', 'h'))
            return self.linear(g.ndata['h'])
    
    model = SimpleGCN(16, 8)
    start_time = time.time()
    output = model(G_dgl, node_features)
    gnn_time = time.time() - start_time
    print(f"   ✅ 图神经网络测试成功 / Graph Neural Network test successful")
    print(f"   输出维度 / Output dimension: {output.shape}")
    print(f"   推理时间 / Inference time: {gnn_time:.4f}s")
    
except Exception as e:
    print(f"   ❌ 图神经网络测试失败 / Graph Neural Network test failed: {e}")
print()

# 6. 性能基准 / Performance Benchmark
print("6. 性能基准 / Performance Benchmark")

# 大规模图测试 / Large-scale graph test
sizes = [100, 500, 1000]
creation_times = []
algorithm_times = []

for size in sizes:
    # 创建图 / Create graph
    start_time = time.time()
    G_large = nx.erdos_renyi_graph(size, 0.1)
    creation_time = time.time() - start_time
    creation_times.append(creation_time)
    
    # 算法测试 / Algorithm test
    start_time = time.time()
    nx.shortest_path(G_large, 0, min(50, size-1))
    algorithm_time = time.time() - start_time
    algorithm_times.append(algorithm_time)
    
    print(f"   图大小 {size}: 创建 {creation_time:.4f}s, 算法 {algorithm_time:.4f}s")

print()
print(f"   平均创建时间 / Average creation time: {np.mean(creation_times):.4f}s")
print(f"   平均算法时间 / Average algorithm time: {np.mean(algorithm_times):.4f}s")

# 保存结果 / Save results
results = {
    "evaluation_time": datetime.now().isoformat(),
    "environment": {
        "python_version": torch.version.__version__,
        "pytorch_version": torch.__version__,
        "dgl_version": dgl.__version__,
        "networkx_version": nx.__version__,
        "cuda_available": torch.cuda.is_available()
    },
    "tests": {
        "networkx_graph": "PASS" if 'G_nx' in locals() else "FAIL",
        "dgl_graph": "PASS" if 'G_dgl' in locals() else "FAIL",
        "graph_algorithms": "PASS" if 'path' in locals() else "FAIL",
        "gnn_test": "PASS" if 'output' in locals() else "FAIL"
    },
    "performance": {
        "graph_sizes": sizes,
        "creation_times": creation_times,
        "algorithm_times": algorithm_times,
        "average_creation_time": float(np.mean(creation_times)),
        "average_algorithm_time": float(np.mean(algorithm_times))
    }
}

with open("results/graph-theory/evaluation_results.json", "w", encoding="utf-8") as f:
    json.dump(results, f, ensure_ascii=False, indent=2)

print()
print("=== 评测完成 / Evaluation Completed ===")
print(f"结果已保存到: results/graph-theory/evaluation_results.json")
EOF

    log_success "图论评测完成 / Graph Theory evaluation completed"
}

# 显示结果 / Show Results
show_results() {
    log_info "评测结果 / Evaluation Results:"
    
    if [ -f "results/graph-theory/evaluation_results.json" ]; then
        echo ""
        echo "📊 详细结果 / Detailed Results:"
        cat results/graph-theory/evaluation_results.json | python -m json.tool
    else
        log_warning "未找到结果文件 / Results file not found"
    fi
}

# 主函数 / Main function
main() {
    log_info "开始图论模块评测 / Starting Graph Theory module evaluation"
    echo ""
    
    # 检查环境 / Check environment
    check_environment
    
    # 运行评测 / Run evaluation
    run_evaluation
    
    # 显示结果 / Show results
    show_results
    
    log_success "图论模块评测完成！/ Graph Theory module evaluation completed!"
}

# 错误处理 / Error handling
trap 'log_error "评测执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
