# 基础镜像 / Base Image
FROM ubuntu:22.04

# 设置环境变量 / Set Environment Variables
ENV DEBIAN_FRONTEND=noninteractive
ENV PYTHONUNBUFFERED=1
ENV PYTHONDONTWRITEBYTECODE=1
ENV PIP_NO_CACHE_DIR=1
ENV PIP_DISABLE_PIP_VERSION_CHECK=1

# 设置工作目录 / Set Working Directory
WORKDIR /workspace

# 安装系统依赖 / Install System Dependencies
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    git \
    vim \
    nano \
    htop \
    tree \
    unzip \
    build-essential \
    cmake \
    pkg-config \
    libssl-dev \
    libffi-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    libncurses5-dev \
    libncursesw5-dev \
    libgdbm-dev \
    libnss3-dev \
    libxml2-dev \
    libxslt-dev \
    libjpeg-dev \
    libpng-dev \
    libfreetype6-dev \
    liblcms2-dev \
    libopenjp2-7-dev \
    libtiff5-dev \
    zlib1g-dev \
    libopenblas-dev \
    liblapack-dev \
    libatlas-base-dev \
    gfortran \
    && rm -rf /var/lib/apt/lists/*

# 安装Python 3.11 / Install Python 3.11
RUN apt-get update && apt-get install -y \
    python3.11 \
    python3.11-dev \
    python3.11-distutils \
    python3.11-venv \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

# 创建Python符号链接 / Create Python Symbolic Links
RUN ln -sf /usr/bin/python3.11 /usr/bin/python3
RUN ln -sf /usr/bin/python3.11 /usr/bin/python

# 升级pip / Upgrade pip
RUN python -m pip install --upgrade pip setuptools wheel

# 安装常用Python包 / Install Common Python Packages
RUN pip install \
    numpy \
    pandas \
    scipy \
    matplotlib \
    seaborn \
    jupyter \
    ipython \
    requests \
    beautifulsoup4 \
    lxml \
    pillow \
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

# 设置用户 / Set User
RUN useradd -m -s /bin/bash kguser && \
    chown -R kguser:kguser /workspace
USER kguser

# 设置Python路径 / Set Python Path
ENV PYTHONPATH=/workspace:$PYTHONPATH

# 默认命令 / Default Command
CMD ["/bin/bash"]
