# 知识表示环境 / Knowledge Representation Environment
FROM ghcr.io/kg/base:latest

# 安装知识图谱相关包 / Install Knowledge Graph Packages
RUN pip install \
    torch \
    torchvision \
    torchaudio \
    transformers \
    sentence-transformers \
    gensim \
    networkx \
    rdflib \
    owlready2 \
    pymongo \
    neo4j \
    redis \
    elasticsearch \
    faiss-cpu \
    scikit-learn \
    pandas \
    numpy \
    scipy \
    matplotlib \
    seaborn \
    plotly \
    streamlit \
    gradio

# 设置环境变量 / Set Environment Variables
ENV TRANSFORMERS_CACHE=/workspace/.cache/huggingface
ENV TORCH_HOME=/workspace/.cache/torch

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/data /workspace/models /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
