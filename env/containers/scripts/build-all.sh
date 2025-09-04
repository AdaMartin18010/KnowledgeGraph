#!/bin/bash

# 构建所有知识图谱评测环境镜像 / Build All Knowledge Graph Evaluation Environment Images
# 作者 / Author: KnowledgeGraph Team
# 版本 / Version: 1.0.0

set -e

# 颜色定义 / Color Definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 日志函数 / Logging Functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 检查Docker是否运行 / Check if Docker is running
check_docker() {
    if ! docker info > /dev/null 2>&1; then
        log_error "Docker is not running. Please start Docker and try again."
        exit 1
    fi
    log_success "Docker is running"
}

# 构建基础镜像 / Build base image
build_base() {
    log_info "Building base image..."
    docker build -f dockerfiles/base/Dockerfile -t ghcr.io/kg/base:latest dockerfiles/base/
    log_success "Base image built successfully"
}

# 构建知识表示环境 / Build Knowledge Representation environment
build_kr() {
    log_info "Building Knowledge Representation environment..."
    docker build -f dockerfiles/knowledge-representation/Dockerfile -t ghcr.io/kg/kr-eval:1.0.0 dockerfiles/knowledge-representation/
    log_success "Knowledge Representation environment built successfully"
}

# 构建图论环境 / Build Graph Theory environment
build_gt() {
    log_info "Building Graph Theory environment..."
    docker build -f dockerfiles/graph-theory/Dockerfile -t ghcr.io/kg/gt-eval:1.0.0 dockerfiles/graph-theory/
    log_success "Graph Theory environment built successfully"
}

# 构建语义分析环境 / Build Semantic Analysis environment
build_sa() {
    log_info "Building Semantic Analysis environment..."
    docker build -f dockerfiles/semantic-analysis/Dockerfile -t ghcr.io/kg/sa-eval:1.0.0 dockerfiles/semantic-analysis/
    log_success "Semantic Analysis environment built successfully"
}

# 构建本体工程环境 / Build Ontology Engineering environment
build_oe() {
    log_info "Building Ontology Engineering environment..."
    docker build -f dockerfiles/ontology-engineering/Dockerfile -t ghcr.io/kg/oe-eval:1.0.0 dockerfiles/ontology-engineering/
    log_success "Ontology Engineering environment built successfully"
}

# 构建知识抽取环境 / Build Knowledge Extraction environment
build_ke() {
    log_info "Building Knowledge Extraction environment..."
    docker build -f dockerfiles/knowledge-extraction/Dockerfile -t ghcr.io/kg/ke-eval:1.0.0 dockerfiles/knowledge-extraction/
    log_success "Knowledge Extraction environment built successfully"
}

# 构建推理系统环境 / Build Reasoning Systems environment
build_rs() {
    log_info "Building Reasoning Systems environment..."
    docker build -f dockerfiles/reasoning-systems/Dockerfile -t ghcr.io/kg/rs-eval:1.0.0 dockerfiles/reasoning-systems/
    log_success "Reasoning Systems environment built successfully"
}

# 构建应用环境 / Build Applications environment
build_app() {
    log_info "Building Applications environment..."
    docker build -f dockerfiles/applications/Dockerfile -t ghcr.io/kg/app-eval:1.0.0 dockerfiles/applications/
    log_success "Applications environment built successfully"
}

# 构建形式化方法环境 / Build Formal Methods environment
build_fm() {
    log_info "Building Formal Methods environment..."
    docker build -f dockerfiles/formal-methods/Dockerfile -t ghcr.io/kg/fm-eval:1.0.0 dockerfiles/formal-methods/
    log_success "Formal Methods environment built successfully"
}

# 构建工程实践环境 / Build Engineering Practice environment
build_ep() {
    log_info "Building Engineering Practice environment..."
    docker build -f dockerfiles/engineering-practice/Dockerfile -t ghcr.io/kg/ep-eval:1.0.0 dockerfiles/engineering-practice/
    log_success "Engineering Practice environment built successfully"
}

# 构建研究方法论环境 / Build Research Methodology environment
build_rm() {
    log_info "Building Research Methodology environment..."
    docker build -f dockerfiles/research-methodology/Dockerfile -t ghcr.io/kg/rm-eval:1.0.0 dockerfiles/research-methodology/
    log_success "Research Methodology environment built successfully"
}

# 显示镜像列表 / Show image list
show_images() {
    log_info "Built images:"
    docker images | grep "ghcr.io/kg"
}

# 主函数 / Main function
main() {
    log_info "Starting to build all Knowledge Graph evaluation environments..."
    
    # 检查Docker / Check Docker
    check_docker
    
    # 构建所有镜像 / Build all images
    build_base
    build_kr
    build_gt
    build_sa
    build_oe
    build_ke
    build_rs
    build_app
    build_fm
    build_ep
    build_rm
    
    # 显示结果 / Show results
    show_images
    
    log_success "All environments built successfully!"
    log_info "You can now run: docker-compose -f docker-compose/evaluation.yml up -d"
}

# 错误处理 / Error handling
trap 'log_error "Build failed. Please check the error messages above."' ERR

# 执行主函数 / Execute main function
main "$@"
