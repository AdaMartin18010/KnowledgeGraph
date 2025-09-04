# 图论环境 / Graph Theory Environment
FROM ghcr.io/kg/base:latest

# 安装图论相关包 / Install Graph Theory Packages
RUN pip install \
    torch \
    torchvision \
    torchaudio \
    dgl \
    dgl-cu118 \
    networkx \
    matplotlib \
    seaborn \
    plotly \
    scipy \
    numpy \
    pandas \
    scikit-learn \
    scikit-image \
    opencv-python \
    tqdm \
    click \
    pyyaml \
    toml \
    python-dotenv \
    psutil \
    memory-profiler \
    line-profiler

# 设置环境变量 / Set Environment Variables
ENV DGL_HOME=/workspace/.cache/dgl
ENV TORCH_HOME=/workspace/.cache/torch

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/data /workspace/models /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
