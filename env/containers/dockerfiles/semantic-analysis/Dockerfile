# 语义分析环境 / Semantic Analysis Environment
FROM ghcr.io/kg/base:latest

# 安装语义分析相关包 / Install Semantic Analysis Packages
RUN pip install \
    torch \
    torchvision \
    torchaudio \
    transformers \
    sentence-transformers \
    spacy \
    nltk \
    gensim \
    scikit-learn \
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

# 下载spaCy模型 / Download spaCy Models
RUN python -m spacy download en_core_web_sm
RUN python -m spacy download zh_core_web_sm

# 设置环境变量 / Set Environment Variables
ENV TRANSFORMERS_CACHE=/workspace/.cache/huggingface
ENV TORCH_HOME=/workspace/.cache/torch
ENV SPACY_DATA=/workspace/.cache/spacy

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/data /workspace/models /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
