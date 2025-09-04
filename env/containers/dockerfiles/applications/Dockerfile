# 应用环境 / Applications Environment
FROM ghcr.io/kg/base:latest

# 安装应用相关包 / Install Applications Packages
RUN pip install \
    flask \
    fastapi \
    uvicorn \
    streamlit \
    gradio \
    dash \
    plotly \
    pandas \
    numpy \
    scipy \
    scikit-learn \
    transformers \
    sentence-transformers \
    redis \
    pymongo \
    elasticsearch \
    requests \
    beautifulsoup4 \
    lxml \
    tqdm \
    click \
    pyyaml \
    toml \
    python-dotenv \
    psutil \
    memory-profiler \
    line-profiler

# 设置环境变量 / Set Environment Variables
ENV FLASK_ENV=development
ENV FLASK_APP=app.py
ENV TRANSFORMERS_CACHE=/workspace/.cache/huggingface

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/apps /workspace/data /workspace/models /workspace/results

# 暴露端口 / Expose Ports
EXPOSE 5000 8501 8000

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
