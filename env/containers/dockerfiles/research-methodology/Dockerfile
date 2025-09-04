# 研究方法论环境 / Research Methodology Environment
FROM ghcr.io/kg/base:latest

# 安装研究方法论相关包 / Install Research Methodology Packages
RUN pip install \
    pandas \
    numpy \
    scipy \
    matplotlib \
    seaborn \
    plotly \
    bokeh \
    altair \
    statsmodels \
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

# 安装R语言 / Install R Language
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    r-cran-ggplot2 \
    r-cran-dplyr \
    r-cran-tidyr \
    r-cran-rmarkdown \
    r-cran-knitr \
    && rm -rf /var/lib/apt/lists/*

# 设置环境变量 / Set Environment Variables
ENV R_LIBS=/workspace/.R/library
ENV PYTHONPATH=/workspace:$PYTHONPATH

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/experiments /workspace/analysis /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
