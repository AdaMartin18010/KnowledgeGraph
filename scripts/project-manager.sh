#!/bin/bash

# 知识图谱项目综合管理脚本 / Knowledge Graph Project Comprehensive Management Script
# 作者 / Author: KnowledgeGraph Team
# 版本 / Version: 1.0.0

set -e

# 颜色定义 / Color Definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
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

log_step() {
    echo -e "${CYAN}[STEP]${NC} $1"
}

log_highlight() {
    echo -e "${MAGENTA}[HIGHLIGHT]${NC} $1"
}

# 显示帮助信息 / Show Help Information
show_help() {
    echo "知识图谱项目综合管理脚本 / Knowledge Graph Project Management Script"
    echo "================================================================"
    echo ""
    echo "用法 / Usage: $0 [选项] [命令]"
    echo ""
    echo "选项 / Options:"
    echo "  -h, --help     显示此帮助信息 / Show this help message"
    echo "  -v, --version  显示版本信息 / Show version information"
    echo ""
    echo "命令 / Commands:"
    echo "  status         显示项目状态 / Show project status"
    echo "  health         运行健康检查 / Run health check"
    echo "  stats          生成统计报告 / Generate statistics report"
    echo "  check          运行完成度检查 / Run completion check"
    echo "  eval           运行综合评测 / Run comprehensive evaluation"
    echo "  build          构建环境 / Build environment"
    echo "  start          启动环境 / Start environment"
    echo "  stop           停止环境 / Stop environment"
    echo "  clean          清理环境 / Clean environment"
    echo "  backup         备份项目 / Backup project"
    echo "  restore        恢复项目 / Restore project"
    echo "  update         更新项目 / Update project"
    echo "  report         生成综合报告 / Generate comprehensive report"
    echo ""
    echo "示例 / Examples:"
    echo "  $0 status       # 显示项目状态"
    echo "  $0 health       # 运行健康检查"
    echo "  $0 build        # 构建环境"
    echo "  $0 start        # 启动环境"
    echo ""
}

# 显示版本信息 / Show Version Information
show_version() {
    echo "知识图谱项目综合管理脚本 v1.0.0"
    echo "Knowledge Graph Project Management Script v1.0.0"
    echo "作者 / Author: KnowledgeGraph Team"
    echo "最后更新 / Last Updated: 2025-01-01"
}

# 显示项目状态 / Show Project Status
show_project_status() {
    log_step "显示项目状态 / Showing Project Status"
    
    echo ""
    echo "🏗️  知识图谱项目状态 / Knowledge Graph Project Status"
    echo "=================================================="
    echo ""
    
    # 基本信息 / Basic information
    echo "📋 基本信息 / Basic Information:"
    echo "   项目名称 / Project Name: 知识图谱项目 / Knowledge Graph Project"
    echo "   项目版本 / Project Version: v1.0.0"
    echo "   最后更新 / Last Updated: 2025-01-01"
    echo "   维护者 / Maintainer: KnowledgeGraph Team"
    echo ""
    
    # 文件统计 / File statistics
    echo "📁 文件统计 / File Statistics:"
    local markdown_files=$(find . -name "*.md" 2>/dev/null | wc -l)
    local shell_scripts=$(find . -name "*.sh" 2>/dev/null | wc -l)
    local docker_files=$(find . -name "Dockerfile" 2>/dev/null | wc -l)
    echo "   Markdown文件 / Markdown Files: $markdown_files"
    echo "   Shell脚本 / Shell Scripts: $shell_scripts"
    echo "   Docker文件 / Docker Files: $docker_files"
    echo ""
    
    # 模块状态 / Module status
    echo "📚 核心模块状态 / Core Modules Status:"
    for i in {01..10}; do
        case $i in
            01) module_name="知识表示" ;;
            02) module_name="图论" ;;
            03) module_name="语义分析" ;;
            04) module_name="本体工程" ;;
            05) module_name="知识抽取" ;;
            06) module_name="推理系统" ;;
            07) module_name="应用" ;;
            08) module_name="形式化方法" ;;
            09) module_name="工程实践" ;;
            10) module_name="研究方法论" ;;
        esac
        
        local readme_exists=""
        local sample_exists=""
        
        if [ -f "docs/$i-*/README.md" ]; then
            readme_exists="✅"
        else
            readme_exists="❌"
        fi
        
        if [ -f "docs/evaluation-reports/$i-*-sample.md" ]; then
            sample_exists="✅"
        else
            sample_exists="❌"
        fi
        
        echo "   模块 $i ($module_name): README $readme_exists, 示例 $sample_exists"
    done
    
    echo ""
    echo "=================================================="
}

# 运行健康检查 / Run Health Check
run_health_check() {
    log_step "运行项目健康检查 / Running Project Health Check"
    
    if [ -f "scripts/health-check.sh" ]; then
        bash scripts/health-check.sh
    else
        log_error "健康检查脚本未找到 / Health check script not found"
        exit 1
    fi
}

# 生成统计报告 / Generate Statistics Report
generate_statistics_report() {
    log_step "生成项目统计报告 / Generating Project Statistics Report"
    
    if [ -f "scripts/project-stats.sh" ]; then
        bash scripts/project-stats.sh
    else
        log_error "统计报告脚本未找到 / Statistics script not found"
        exit 1
    fi
}

# 运行完成度检查 / Run Completion Check
run_completion_check() {
    log_step "运行项目完成度检查 / Running Project Completion Check"
    
    if [ -f "scripts/project-completion-check.sh" ]; then
        bash scripts/project-completion-check.sh
    else
        log_error "完成度检查脚本未找到 / Completion check script not found"
        exit 1
    fi
}

# 运行综合评测 / Run Comprehensive Evaluation
run_comprehensive_evaluation() {
    log_step "运行综合评测 / Running Comprehensive Evaluation"
    
    if [ -f "scripts/run-all-evaluations.sh" ]; then
        bash scripts/run-all-evaluations.sh
    else
        log_error "综合评测脚本未找到 / Comprehensive evaluation script not found"
        exit 1
    fi
}

# 构建环境 / Build Environment
build_environment() {
    log_step "构建项目环境 / Building Project Environment"
    
    # 检查Docker是否可用 / Check if Docker is available
    if ! command -v docker &> /dev/null; then
        log_error "Docker未安装或不在PATH中 / Docker not installed or not in PATH"
        exit 1
    fi
    
    if ! docker info &> /dev/null; then
        log_error "Docker未运行 / Docker is not running"
        exit 1
    fi
    
    # 构建基础镜像 / Build base image
    log_info "构建基础镜像 / Building base image..."
    docker build -f env/containers/dockerfiles/base/Dockerfile -t ghcr.io/kg/base:latest env/containers/dockerfiles/base/
    
    # 构建所有模块环境 / Build all module environments
    log_info "构建所有模块环境 / Building all module environments..."
    if [ -f "env/containers/scripts/build-all.sh" ]; then
        bash env/containers/scripts/build-all.sh
    else
        log_error "构建脚本未找到 / Build script not found"
        exit 1
    fi
    
    log_success "环境构建完成 / Environment build completed"
}

# 启动环境 / Start Environment
start_environment() {
    log_step "启动项目环境 / Starting Project Environment"
    
    if [ -f "env/containers/docker-compose/evaluation.yml" ]; then
        docker-compose -f env/containers/docker-compose/evaluation.yml up -d
        log_success "环境启动完成 / Environment started successfully"
        
        # 显示运行状态 / Show running status
        echo ""
        echo "🐳 容器运行状态 / Container Running Status:"
        docker ps --filter "name=kg-" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    else
        log_error "Docker Compose配置未找到 / Docker Compose configuration not found"
        exit 1
    fi
}

# 停止环境 / Stop Environment
stop_environment() {
    log_step "停止项目环境 / Stopping Project Environment"
    
    if [ -f "env/containers/docker-compose/evaluation.yml" ]; then
        docker-compose -f env/containers/docker-compose/evaluation.yml down
        log_success "环境停止完成 / Environment stopped successfully"
    else
        log_error "Docker Compose配置未找到 / Docker Compose configuration not found"
        exit 1
    fi
}

# 清理环境 / Clean Environment
clean_environment() {
    log_step "清理项目环境 / Cleaning Project Environment"
    
    # 停止并删除容器 / Stop and remove containers
    if [ -f "env/containers/docker-compose/evaluation.yml" ]; then
        docker-compose -f env/containers/docker-compose/evaluation.yml down -v
    fi
    
    # 删除镜像 / Remove images
    docker images | grep "ghcr.io/kg" | awk '{print $3}' | xargs -r docker rmi -f
    
    # 清理未使用的资源 / Clean unused resources
    docker system prune -f
    
    log_success "环境清理完成 / Environment cleaned successfully"
}

# 备份项目 / Backup Project
backup_project() {
    log_step "备份项目 / Backing Up Project"
    
    local backup_dir="backups/$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$backup_dir"
    
    # 备份核心文件 / Backup core files
    cp -r docs "$backup_dir/"
    cp -r scripts "$backup_dir/"
    cp -r env "$backup_dir/"
    cp -r data "$backup_dir/"
    cp *.md "$backup_dir/"
    cp LICENSE "$backup_dir/"
    
    # 创建备份信息 / Create backup info
    cat > "$backup_dir/backup_info.txt" << EOF
知识图谱项目备份信息 / Knowledge Graph Project Backup Information
===============================================================
备份时间 / Backup Time: $(date)
备份版本 / Backup Version: v1.0.0
备份内容 / Backup Contents: 核心文档、脚本、环境配置、数据管理
备份大小 / Backup Size: $(du -sh "$backup_dir" | cut -f1)
EOF
    
    log_success "项目备份完成: $backup_dir / Project backup completed: $backup_dir"
}

# 恢复项目 / Restore Project
restore_project() {
    log_step "恢复项目 / Restoring Project"
    
    if [ $# -eq 0 ]; then
        log_error "请指定备份目录 / Please specify backup directory"
        echo "用法 / Usage: $0 restore <backup_directory>"
        exit 1
    fi
    
    local backup_dir="$1"
    if [ ! -d "$backup_dir" ]; then
        log_error "备份目录不存在 / Backup directory does not exist: $backup_dir"
        exit 1
    fi
    
    # 恢复文件 / Restore files
    cp -r "$backup_dir"/* .
    
    log_success "项目恢复完成 / Project restore completed"
}

# 更新项目 / Update Project
update_project() {
    log_step "更新项目 / Updating Project"
    
    # 检查Git仓库状态 / Check Git repository status
    if [ -d ".git" ]; then
        log_info "检测到Git仓库，正在更新 / Git repository detected, updating..."
        git pull origin main 2>/dev/null || git pull origin master 2>/dev/null || {
            log_warning "Git更新失败，跳过 / Git update failed, skipping"
        }
    else
        log_warning "未检测到Git仓库，跳过更新 / No Git repository detected, skipping update"
    fi
    
    # 更新依赖 / Update dependencies (如果有的话)
    log_info "项目更新完成 / Project update completed"
}

# 生成综合报告 / Generate Comprehensive Report
generate_comprehensive_report() {
    log_step "生成综合报告 / Generating Comprehensive Report"
    
    # 运行各种检查 / Run various checks
    echo "🔍 正在收集项目信息... / Collecting project information..."
    
    # 创建综合报告目录 / Create comprehensive report directory
    local report_dir="results/comprehensive_report_$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$report_dir"
    
    # 运行各种检查并保存结果 / Run various checks and save results
    if [ -f "scripts/health-check.sh" ]; then
        bash scripts/health-check.sh > "$report_dir/health_check.txt" 2>&1
    fi
    
    if [ -f "scripts/project-stats.sh" ]; then
        bash scripts/project-stats.sh > "$report_dir/statistics.txt" 2>&1
    fi
    
    if [ -f "scripts/project-completion-check.sh" ]; then
        bash scripts/project-completion-check.sh > "$report_dir/completion_check.txt" 2>&1
    fi
    
    # 生成综合报告 / Generate comprehensive report
    cat > "$report_dir/comprehensive_report.md" << EOF
# 知识图谱项目综合报告 / Knowledge Graph Project Comprehensive Report

## 📅 报告生成时间 / Report Generation Time
$(date)

## 📊 报告内容 / Report Contents

本报告包含以下检查结果：
- 项目健康检查 / Project Health Check
- 项目统计信息 / Project Statistics
- 项目完成度检查 / Project Completion Check

## 🔍 详细结果 / Detailed Results

请查看以下文件：
- \`health_check.txt\`: 健康检查结果
- \`statistics.txt\`: 统计信息结果
- \`completion_check.txt\`: 完成度检查结果

## 📁 报告位置 / Report Location
$report_dir

---
生成者 / Generated by: 项目综合管理脚本 / Project Management Script
EOF
    
    log_success "综合报告生成完成: $report_dir / Comprehensive report generated: $report_dir"
}

# 主函数 / Main function
main() {
    # 解析命令行参数 / Parse command line arguments
    if [ $# -eq 0 ]; then
        show_help
        exit 0
    fi
    
    case "$1" in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--version)
            show_version
            exit 0
            ;;
        status)
            show_project_status
            ;;
        health)
            run_health_check
            ;;
        stats)
            generate_statistics_report
            ;;
        check)
            run_completion_check
            ;;
        eval)
            run_comprehensive_evaluation
            ;;
        build)
            build_environment
            ;;
        start)
            start_environment
            ;;
        stop)
            stop_environment
            ;;
        clean)
            clean_environment
            ;;
        backup)
            backup_project
            ;;
        restore)
            restore_project "$2"
            ;;
        update)
            update_project
            ;;
        report)
            generate_comprehensive_report
            ;;
        *)
            log_error "未知命令 / Unknown command: $1"
            echo ""
            show_help
            exit 1
            ;;
    esac
}

# 错误处理 / Error handling
trap 'log_error "项目管理脚本执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
