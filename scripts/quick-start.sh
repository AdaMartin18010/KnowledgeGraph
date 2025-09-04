#!/bin/bash

# 知识图谱项目快速启动脚本 / Knowledge Graph Project Quick Start Script
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

# 检查系统要求 / Check System Requirements
check_requirements() {
    log_info "检查系统要求 / Checking system requirements..."
    
    # 检查Docker / Check Docker
    if ! command -v docker &> /dev/null; then
        log_error "Docker未安装。请先安装Docker。"
        log_info "安装指南: https://docs.docker.com/get-docker/"
        exit 1
    fi
    
    # 检查Docker Compose / Check Docker Compose
    if ! command -v docker-compose &> /dev/null; then
        log_error "Docker Compose未安装。请先安装Docker Compose。"
        log_info "安装指南: https://docs.docker.com/compose/install/"
        exit 1
    fi
    
    # 检查Docker是否运行 / Check if Docker is running
    if ! docker info &> /dev/null; then
        log_error "Docker未运行。请启动Docker服务。"
        exit 1
    fi
    
    log_success "系统要求检查通过 / System requirements check passed"
}

# 构建环境 / Build Environment
build_environment() {
    log_info "开始构建知识图谱评测环境 / Building Knowledge Graph evaluation environment..."
    
    # 构建基础镜像 / Build base image
    log_info "构建基础镜像 / Building base image..."
    docker build -f env/containers/dockerfiles/base/Dockerfile -t ghcr.io/kg/base:latest env/containers/dockerfiles/base/
    
    # 构建所有专业环境 / Build all specialized environments
    log_info "构建专业环境 / Building specialized environments..."
    bash env/containers/scripts/build-all.sh
    
    log_success "环境构建完成 / Environment build completed"
}

# 启动评测环境 / Start Evaluation Environment
start_environment() {
    log_info "启动评测环境 / Starting evaluation environment..."
    
    # 使用Docker Compose启动 / Start with Docker Compose
    docker-compose -f env/containers/docker-compose/evaluation.yml up -d
    
    log_success "评测环境启动完成 / Evaluation environment started"
}

# 显示环境状态 / Show Environment Status
show_status() {
    log_info "环境状态 / Environment status:"
    
    # 显示运行中的容器 / Show running containers
    echo "运行中的容器 / Running containers:"
    docker ps --filter "name=kg-" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    
    echo ""
    echo "容器网络 / Container network:"
    docker network ls | grep kg-network || echo "网络未创建 / Network not created"
}

# 运行示例评测 / Run Sample Evaluation
run_sample_evaluation() {
    log_info "运行示例评测 / Running sample evaluation..."
    
    # 进入知识表示环境运行示例 / Enter Knowledge Representation environment and run sample
    log_info "在知识表示环境中运行示例 / Running sample in Knowledge Representation environment..."
    docker exec -it kg-kr-eval bash -c "
        cd /workspace/docs/evaluation-reports &&
        python -c '
import sys
sys.path.append(\"/workspace\")
print(\"示例评测环境准备就绪 / Sample evaluation environment ready\")
print(\"您可以运行: python scripts/kr_eval.sh\")
'
    "
    
    log_success "示例评测完成 / Sample evaluation completed"
}

# 显示使用说明 / Show Usage Instructions
show_usage() {
    log_info "使用说明 / Usage instructions:"
    echo ""
    echo "1. 进入特定环境 / Enter specific environment:"
    echo "   docker exec -it kg-kr-eval bash    # 知识表示环境"
    echo "   docker exec -it kg-gt-eval bash    # 图论环境"
    echo "   docker exec -it kg-sa-eval bash    # 语义分析环境"
    echo ""
    echo "2. 运行评测脚本 / Run evaluation scripts:"
    echo "   bash scripts/kr_eval.sh            # 知识表示评测"
    echo "   bash scripts/gt_eval.sh            # 图论评测"
    echo ""
    echo "3. 查看文档 / View documentation:"
    echo "   docs/KNOWLEDGE_GRAPH_INDEX.md      # 项目索引"
    echo "   docs/evaluation-reports/           # 评测报告"
    echo ""
    echo "4. 停止环境 / Stop environment:"
    echo "   docker-compose -f env/containers/docker-compose/evaluation.yml down"
}

# 主函数 / Main function
main() {
    log_info "欢迎使用知识图谱项目快速启动脚本 / Welcome to Knowledge Graph Project Quick Start Script"
    echo ""
    
    # 检查系统要求 / Check system requirements
    check_requirements
    
    # 询问是否构建环境 / Ask if build environment
    echo ""
    read -p "是否构建评测环境？(y/n) / Build evaluation environment? (y/n): " -n 1 -r
    echo ""
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        build_environment
    else
        log_info "跳过环境构建 / Skipping environment build"
    fi
    
    # 询问是否启动环境 / Ask if start environment
    echo ""
    read -p "是否启动评测环境？(y/n) / Start evaluation environment? (y/n): " -n 1 -r
    echo ""
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        start_environment
        show_status
        
        # 询问是否运行示例 / Ask if run sample
        echo ""
        read -p "是否运行示例评测？(y/n) / Run sample evaluation? (y/n): " -n 1 -r
        echo ""
        
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            run_sample_evaluation
        fi
    fi
    
    # 显示使用说明 / Show usage instructions
    echo ""
    show_usage
    
    log_success "快速启动完成！/ Quick start completed!"
    log_info "更多信息请查看: docs/KNOWLEDGE_GRAPH_INDEX.md"
}

# 错误处理 / Error handling
trap 'log_error "脚本执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
