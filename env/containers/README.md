# 环境容器配置 / Environment Container Configuration

## 概述 / Overview

本目录包含知识图谱项目运行和评测所需的各种Docker容器配置，确保环境的一致性和可复现性。

## 目录结构 / Directory Structure

```text
env/containers/
├── README.md                    # 本说明文档
├── dockerfiles/                 # Dockerfile定义
│   ├── base/                    # 基础镜像
│   ├── knowledge-representation/  # 知识表示环境
│   ├── graph-theory/             # 图论环境
│   ├── semantic-analysis/        # 语义分析环境
│   ├── ontology-engineering/     # 本体工程环境
│   ├── knowledge-extraction/     # 知识抽取环境
│   ├── reasoning-systems/        # 推理系统环境
│   ├── applications/             # 应用环境
│   ├── formal-methods/           # 形式化方法环境
│   ├── engineering-practice/     # 工程实践环境
│   └── research-methodology/     # 研究方法论环境
├── docker-compose/              # Docker Compose配置
│   ├── development.yml           # 开发环境
│   ├── evaluation.yml            # 评测环境
│   └── production.yml            # 生产环境
├── scripts/                     # 容器管理脚本
│   ├── build-all.sh             # 构建所有镜像
│   ├── run-evaluation.sh        # 运行评测环境
│   └── cleanup.sh               # 清理容器和镜像
└── configs/                     # 配置文件
    ├── environment/              # 环境变量配置
    ├── volumes/                  # 卷挂载配置
    └── networks/                 # 网络配置
```

## 容器分类 / Container Categories

### 1. 基础环境 / Base Environment

- **基础镜像**: Ubuntu 22.04 + Python 3.11 + 常用工具
- **用途**: 所有其他容器的父镜像
- **标签**: `ghcr.io/kg/base:latest`

### 2. 知识表示 / Knowledge Representation

- **环境**: PyTorch + Transformers + 知识图谱库
- **用途**: 知识表示学习、嵌入训练
- **标签**: `ghcr.io/kg/kr-eval:1.0.0`

### 3. 图论 / Graph Theory

- **环境**: PyTorch + DGL + NetworkX + 图算法库
- **用途**: 图算法实现、图神经网络训练
- **标签**: `ghcr.io/kg/gt-eval:1.0.0`

### 4. 语义分析 / Semantic Analysis

- **环境**: PyTorch + Transformers + spaCy + NLTK
- **用途**: 语义角色标注、跨语言对齐
- **标签**: `ghcr.io/kg/sa-eval:1.0.0`

### 5. 本体工程 / Ontology Engineering

- **环境**: Java + Protégé + OWL API + 推理引擎
- **用途**: 本体构建、一致性检查、推理验证
- **标签**: `ghcr.io/kg/oe-eval:1.0.0`

### 6. 知识抽取 / Knowledge Extraction

- **环境**: PyTorch + Transformers + 序列标注库
- **用途**: 实体关系抽取、事件抽取
- **标签**: `ghcr.io/kg/ke-eval:1.0.0`

### 7. 推理系统 / Reasoning Systems

- **环境**: PyTorch + DGL + 符号推理库
- **用途**: 知识图谱推理、链接预测
- **标签**: `ghcr.io/kg/rs-eval:1.0.0`

### 8. 应用 / Applications

- **环境**: Flask + FastAPI + 前端框架
- **用途**: 问答系统、推荐系统、搜索系统
- **标签**: `ghcr.io/kg/app-eval:1.0.0`

### 9. 形式化方法 / Formal Methods

- **环境**: Coq + Isabelle + Lean + 模型检查器
- **用途**: 形式化验证、定理证明
- **标签**: `ghcr.io/kg/fm-eval:1.0.0`

### 10. 工程实践 / Engineering Practice

- **环境**: Kubernetes + 监控工具 + 性能测试工具
- **用途**: 系统部署、性能评测、监控
- **标签**: `ghcr.io/kg/ep-eval:1.0.0`

### 11. 研究方法论 / Research Methodology

- **环境**: Python + R + 统计库 + 可视化库
- **用途**: 实验设计、统计分析、结果可视化
- **标签**: `ghcr.io/kg/rm-eval:1.0.0`

## 使用说明 / Usage Instructions

### 构建镜像 / Building Images

```bash
# 构建所有镜像
bash env/containers/scripts/build-all.sh

# 构建特定模块镜像
docker build -f env/containers/dockerfiles/knowledge-representation/Dockerfile \
  -t ghcr.io/kg/kr-eval:1.0.0 .
```

### 运行评测环境 / Running Evaluation Environment

```bash
# 使用Docker Compose运行
docker-compose -f env/containers/docker-compose/evaluation.yml up -d

# 或直接运行容器
docker run -it --gpus all -v $(pwd):/workspace \
  ghcr.io/kg/kr-eval:1.0.0 bash
```

### 环境快照 / Environment Snapshots

```bash
# 生成环境快照
docker run --rm ghcr.io/kg/kr-eval:1.0.0 \
  bash -c "pip freeze > requirements.txt && cat requirements.txt" > \
  env/containers/configs/environment/kr-eval-1.0.0.txt
```

## 配置说明 / Configuration Details

### 环境变量 / Environment Variables

- `PYTHONPATH`: Python模块搜索路径
- `CUDA_VISIBLE_DEVICES`: 可见GPU设备
- `OMP_NUM_THREADS`: OpenMP线程数
- `MKL_NUM_THREADS`: MKL线程数

### 卷挂载 / Volume Mounts

- `/workspace`: 项目代码目录
- `/data`: 数据集目录
- `/results`: 结果输出目录
- `/cache`: 缓存目录

### 网络配置 / Network Configuration

- 内部网络: `kg-network`
- 端口映射: 根据服务需要配置
- 服务发现: 使用Docker DNS

## 性能优化 / Performance Optimization

### GPU支持 / GPU Support

- 使用NVIDIA Container Runtime
- 支持多GPU训练和推理
- 自动GPU内存管理

### 资源限制 / Resource Limits

- CPU限制: 根据容器类型配置
- 内存限制: 防止OOM
- 存储限制: 控制磁盘使用

## 安全考虑 / Security Considerations

### 镜像安全 / Image Security

- 使用官方基础镜像
- 定期更新依赖包
- 扫描安全漏洞

### 运行时安全 / Runtime Security

- 非root用户运行
- 最小权限原则
- 网络隔离

## 维护和更新 / Maintenance and Updates

### 定期更新 / Regular Updates

- 每月更新基础镜像
- 季度更新依赖包
- 年度版本升级

### 版本管理 / Version Management

- 语义化版本号
- 变更日志记录
- 向后兼容性保证

## 故障排除 / Troubleshooting

### 常见问题 / Common Issues

1. **GPU不可见**: 检查NVIDIA Container Runtime
2. **内存不足**: 调整容器内存限制
3. **网络连接**: 检查Docker网络配置
4. **权限问题**: 检查卷挂载权限

### 调试命令 / Debug Commands

```bash
# 查看容器日志
docker logs <container_name>

# 进入运行中的容器
docker exec -it <container_name> bash

# 检查容器资源使用
docker stats <container_name>
```

---

**最后更新** / Last Updated: 2025-01-01
**版本** / Version: v1.0.0
