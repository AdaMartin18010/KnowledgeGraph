# 形式化方法环境 / Formal Methods Environment
FROM ghcr.io/kg/base:latest

# 安装形式化方法相关包 / Install Formal Methods Packages
RUN pip install \
    coq \
    isabelle \
    lean \
    z3-solver \
    pysmt \
    sympy \
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
    coq \
    isabelle \
    lean \
    && rm -rf /var/lib/apt/lists/*

# 设置环境变量 / Set Environment Variables
ENV COQPATH=/workspace
ENV ISABELLE_HOME=/usr/share/isabelle
ENV LEAN_PATH=/workspace

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/proofs /workspace/models /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
