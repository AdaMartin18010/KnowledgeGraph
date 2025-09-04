# 本体工程环境 / Ontology Engineering Environment
FROM ghcr.io/kg/base:latest

# 安装Java 11 / Install Java 11
RUN apt-get update && apt-get install -y \
    openjdk-11-jdk \
    openjdk-11-jre \
    maven \
    && rm -rf /var/lib/apt/lists/*

# 设置Java环境变量 / Set Java Environment Variables
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
ENV PATH=$JAVA_HOME/bin:$PATH

# 安装本体工程相关包 / Install Ontology Engineering Packages
RUN pip install \
    rdflib \
    owlready2 \
    pyshacl \
    pysparql \
    sparqlwrapper \
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

# 设置环境变量 / Set Environment Variables
ENV PYTHONPATH=/workspace:$PYTHONPATH

# 创建工作目录 / Create Working Directory
RUN mkdir -p /workspace/data /workspace/ontologies /workspace/results

# 设置默认命令 / Set Default Command
CMD ["/bin/bash"]
