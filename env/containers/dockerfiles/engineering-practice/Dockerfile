# 工程实践环境 / Engineering Practice Environment
FROM ghcr.io/kg/base:latest

# 安装工程实践相关包 / Install Engineering Practice Packages
RUN pip install \
    kubernetes \
    docker \
    prometheus-client \
    grafana-api \
    locust \
    pytest \
    pytest-benchmark \
    pandas \
    numpy \
    scipy \
    matplotlib \
    seaborn \
    plotly \
    tqdm \
    click \
    pyyaml \
    toml \
    python-dotenv \
    psutil \
    memory-profiler \
    line-profiler

# 安装系统依赖 / Install System Dependencies
RUN apt-get update && apt-get install -y \
    kubectl \
    docker.io \
    && rm -rf /var/lib/apt/lists/*

# 设置环境变量 / Set Environment Variables
ENV KUBECONFIG=/workspace/.kube/config
ENV DOCKER_HOST=unix:///var/run/docker.sock

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/deployments /workspace/monitoring /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
