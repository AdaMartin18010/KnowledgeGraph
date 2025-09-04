#!/bin/bash

# 知识图谱项目统计报告脚本 / Knowledge Graph Project Statistics Report Script
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

# 统计计数器 / Statistics Counters
TOTAL_FILES=0
TOTAL_LINES=0
TOTAL_SIZE=0
MARKDOWN_FILES=0
PYTHON_FILES=0
SHELL_FILES=0
DOCKER_FILES=0
YAML_FILES=0

# 统计函数 / Statistics Functions
count_files() {
    local pattern="$1"
    local count=$(find . -name "$pattern" 2>/dev/null | wc -l)
    echo $count
}

count_lines() {
    local pattern="$1"
    local total=0
    while IFS= read -r file; do
        if [ -f "$file" ]; then
            local lines=$(wc -l < "$file" 2>/dev/null || echo "0")
            total=$((total + lines))
        fi
    done < <(find . -name "$pattern" 2>/dev/null)
    echo $total
}

get_file_size() {
    local pattern="$1"
    local total=0
    while IFS= read -r file; do
        if [ -f "$file" ]; then
            local size=$(stat -c%s "$file" 2>/dev/null || echo "0")
            total=$((total + size))
        fi
    done < <(find . -name "$pattern" 2>/dev/null)
    echo $total
}

format_size() {
    local bytes=$1
    if [ $bytes -gt 1048576 ]; then
        echo "$(echo "scale=2; $bytes/1048576" | bc) MB"
    elif [ $bytes -gt 1024 ]; then
        echo "$(echo "scale=2; $bytes/1024" | bc) KB"
    else
        echo "${bytes} B"
    fi
}

# 收集文件统计 / Collect File Statistics
collect_file_statistics() {
    log_step "收集文件统计信息 / Collecting File Statistics"
    
    # 统计各种文件类型 / Count various file types
    MARKDOWN_FILES=$(count_files "*.md")
    PYTHON_FILES=$(count_files "*.py")
    SHELL_FILES=$(count_files "*.sh")
    DOCKER_FILES=$(count_files "Dockerfile")
    YAML_FILES=$(count_files "*.yml")
    
    # 计算总文件数 / Calculate total file count
    TOTAL_FILES=$((MARKDOWN_FILES + PYTHON_FILES + SHELL_FILES + DOCKER_FILES + YAML_FILES))
    
    # 统计行数 / Count lines
    TOTAL_LINES=$(count_lines "*.md")
    TOTAL_LINES=$((TOTAL_LINES + $(count_lines "*.py")))
    TOTAL_LINES=$((TOTAL_LINES + $(count_lines "*.sh")))
    
    # 统计文件大小 / Count file sizes
    TOTAL_SIZE=$(get_file_size "*.md")
    TOTAL_SIZE=$((TOTAL_SIZE + $(get_file_size "*.py")))
    TOTAL_SIZE=$((TOTAL_SIZE + $(get_file_size "*.sh")))
    TOTAL_SIZE=$((TOTAL_SIZE + $(get_file_size "Dockerfile")))
    TOTAL_SIZE=$((TOTAL_SIZE + $(get_file_size "*.yml")))
}

# 分析文档结构 / Analyze Documentation Structure
analyze_documentation() {
    log_step "分析文档结构 / Analyzing Documentation Structure"
    
    echo "📚 文档结构分析 / Documentation Structure Analysis"
    echo "=================================================="
    
    # 核心模块统计 / Core modules statistics
    echo ""
    echo "🏗️  核心模块 / Core Modules:"
    for i in {01..10}; do
        case $i in
            01) module_name="知识表示 / Knowledge Representation" ;;
            02) module_name="图论 / Graph Theory" ;;
            03) module_name="语义分析 / Semantic Analysis" ;;
            04) module_name="本体工程 / Ontology Engineering" ;;
            05) module_name="知识抽取 / Knowledge Extraction" ;;
            06) module_name="推理系统 / Reasoning Systems" ;;
            07) module_name="应用 / Applications" ;;
            08) module_name="形式化方法 / Formal Methods" ;;
            09) module_name="工程实践 / Engineering Practice" ;;
            10) module_name="研究方法论 / Research Methodology" ;;
        esac
        
        local readme_file="docs/$i-*/README.md"
        local readme_count=$(find docs -path "$readme_file" 2>/dev/null | wc -l)
        local sample_file="docs/evaluation-reports/$i-*-sample.md"
        local sample_count=$(find docs/evaluation-reports -path "$sample_file" 2>/dev/null | wc -l)
        
        if [ $readme_count -gt 0 ] && [ $sample_count -gt 0 ]; then
            echo "   ✅ 模块 $i: $module_name"
        else
            echo "   ❌ 模块 $i: $module_name (缺失文件)"
        fi
    done
    
    # 评测报告统计 / Evaluation reports statistics
    echo ""
    echo "📊 评测报告 / Evaluation Reports:"
    local total_reports=$(find docs/evaluation-reports -name "*-sample.md" 2>/dev/null | wc -l)
    echo "   总报告数 / Total Reports: $total_reports"
    
    # 工具和脚本统计 / Tools and scripts statistics
    echo ""
    echo "🛠️  工具和脚本 / Tools and Scripts:"
    local tools_count=$(find docs/tools -name "*.ps1" 2>/dev/null | wc -l)
    local scripts_count=$(find scripts -name "*.sh" 2>/dev/null | wc -l)
    echo "   文档工具 / Documentation Tools: $tools_count"
    echo "   运行脚本 / Runtime Scripts: $scripts_count"
}

# 分析环境配置 / Analyze Environment Configuration
analyze_environment() {
    log_step "分析环境配置 / Analyzing Environment Configuration"
    
    echo ""
    echo "🐳 环境配置分析 / Environment Configuration Analysis"
    echo "=================================================="
    
    # Docker环境统计 / Docker environment statistics
    echo ""
    echo "📦 Docker环境 / Docker Environments:"
    local base_dockerfile=$(find env/containers/dockerfiles -name "Dockerfile" 2>/dev/null | wc -l)
    local module_dockerfiles=$(find env/containers/dockerfiles -path "*/Dockerfile" 2>/dev/null | wc -l)
    echo "   基础镜像 / Base Image: 1"
    echo "   模块环境 / Module Environments: $((module_dockerfiles - base_dockerfile))"
    echo "   总环境数 / Total Environments: $module_dockerfiles"
    
    # 构建脚本统计 / Build script statistics
    echo ""
    echo "🔨 构建脚本 / Build Scripts:"
    local linux_build=$(find env/containers/scripts -name "build-all.sh" 2>/dev/null | wc -l)
    local windows_build=$(find env/containers/scripts -name "build-all.ps1" 2>/dev/null | wc -l)
    echo "   Linux构建脚本 / Linux Build Script: $linux_build"
    echo "   Windows构建脚本 / Windows Build Script: $windows_build"
    
    # Docker Compose配置 / Docker Compose configuration
    echo ""
    echo "🚀 容器编排 / Container Orchestration:"
    local compose_files=$(find env/containers -name "*.yml" 2>/dev/null | wc -l)
    echo "   Compose配置文件 / Compose Files: $compose_files"
}

# 分析数据管理 / Analyze Data Management
analyze_data_management() {
    log_step "分析数据管理 / Analyzing Data Management"
    
    echo ""
    echo "💾 数据管理分析 / Data Management Analysis"
    echo "=========================================="
    
    # 数据快照统计 / Data snapshot statistics
    echo ""
    echo "📸 数据快照 / Data Snapshots:"
    local snapshots_dir="data/snapshots"
    if [ -d "$snapshots_dir" ]; then
        local datasets_dir="$snapshots_dir/datasets"
        local checksums_dir="$snapshots_dir/checksums"
        
        if [ -d "$datasets_dir" ]; then
            local dataset_count=$(find "$datasets_dir" -type d -maxdepth 1 2>/dev/null | wc -l)
            echo "   数据集目录 / Dataset Directories: $((dataset_count - 1))"
        fi
        
        if [ -d "$checksums_dir" ]; then
            local checksum_count=$(find "$checksums_dir" -name "*.sha256" 2>/dev/null | wc -l)
            echo "   校验文件 / Checksum Files: $checksum_count"
        fi
    else
        echo "   ⚠️  数据快照目录不存在 / Data snapshots directory not found"
    fi
}

# 分析代码质量 / Analyze Code Quality
analyze_code_quality() {
    log_step "分析代码质量 / Analyzing Code Quality"
    
    echo ""
    echo "🔍 代码质量分析 / Code Quality Analysis"
    echo "======================================"
    
    # 脚本语法检查 / Script syntax check
    echo ""
    echo "📝 脚本语法 / Script Syntax:"
    local syntax_errors=0
    local total_scripts=0
    
    for script in scripts/*.sh; do
        if [ -f "$script" ]; then
            total_scripts=$((total_scripts + 1))
            if ! bash -n "$script" 2>/dev/null; then
                syntax_errors=$((syntax_errors + 1))
            fi
        fi
    done
    
    echo "   总脚本数 / Total Scripts: $total_scripts"
    echo "   语法错误 / Syntax Errors: $syntax_errors"
    
    if [ $syntax_errors -eq 0 ]; then
        echo "   ✅ 所有脚本语法正确 / All scripts have correct syntax"
    else
        echo "   ❌ 发现语法错误 / Syntax errors found"
    fi
    
    # 文件权限检查 / File permission check
    echo ""
    echo "🔐 文件权限 / File Permissions:"
    local executable_scripts=0
    for script in scripts/*.sh; do
        if [ -f "$script" ] && [ -x "$script" ]; then
            executable_scripts=$((executable_scripts + 1))
        fi
    done
    
    echo "   可执行脚本 / Executable Scripts: $executable_scripts/$total_scripts"
}

# 生成统计报告 / Generate Statistics Report
generate_statistics_report() {
    log_step "生成统计报告 / Generating Statistics Report"
    
    echo ""
    echo "=========================================="
    echo "   📊 项目统计报告 / Project Statistics Report"
    echo "=========================================="
    echo ""
    echo "📈 总体统计 / Overall Statistics:"
    echo "   总文件数 / Total Files: $TOTAL_FILES"
    echo "   总行数 / Total Lines: $TOTAL_LINES"
    echo "   总大小 / Total Size: $(format_size $TOTAL_SIZE)"
    echo ""
    echo "📁 文件类型分布 / File Type Distribution:"
    echo "   Markdown文件 / Markdown Files: $MARKDOWN_FILES"
    echo "   Python文件 / Python Files: $PYTHON_FILES"
    echo "   Shell脚本 / Shell Scripts: $SHELL_FILES"
    echo "   Docker文件 / Docker Files: $DOCKER_FILES"
    echo "   YAML文件 / YAML Files: $YAML_FILES"
    echo ""
    
    # 计算密度指标 / Calculate density metrics
    if [ $TOTAL_FILES -gt 0 ]; then
        local avg_lines_per_file=$((TOTAL_LINES / TOTAL_FILES))
        local avg_size_per_file=$((TOTAL_SIZE / TOTAL_FILES))
        echo "📊 密度指标 / Density Metrics:"
        echo "   平均行数/文件 / Avg Lines per File: $avg_lines_per_file"
        echo "   平均大小/文件 / Avg Size per File: $(format_size $avg_size_per_file)"
    fi
    
    echo ""
    echo "=========================================="
}

# 保存统计结果 / Save Statistics Results
save_statistics() {
    log_step "保存统计结果 / Saving Statistics Results"
    
    # 创建结果目录 / Create results directory
    mkdir -p results
    
    # 生成JSON格式的统计结果 / Generate JSON format statistics
    cat > results/project_statistics.json << EOF
{
  "statistics_time": "$(date -Iseconds)",
  "overall_statistics": {
    "total_files": $TOTAL_FILES,
    "total_lines": $TOTAL_LINES,
    "total_size_bytes": $TOTAL_SIZE,
    "total_size_formatted": "$(format_size $TOTAL_SIZE)"
  },
  "file_type_distribution": {
    "markdown_files": $MARKDOWN_FILES,
    "python_files": $PYTHON_FILES,
    "shell_files": $SHELL_FILES,
    "docker_files": $DOCKER_FILES,
    "yaml_files": $YAML_FILES
  },
  "density_metrics": {
    "avg_lines_per_file": $((TOTAL_LINES / TOTAL_FILES)),
    "avg_size_per_file_bytes": $((TOTAL_SIZE / TOTAL_FILES)),
    "avg_size_per_file_formatted": "$(format_size $((TOTAL_SIZE / TOTAL_FILES)))"
  }
}
EOF
    
    log_success "统计结果已保存到: results/project_statistics.json"
}

# 主函数 / Main function
main() {
    log_info "开始知识图谱项目统计报告 / Starting Knowledge Graph Project statistics report"
    echo ""
    
    # 收集文件统计 / Collect file statistics
    collect_file_statistics
    
    # 分析文档结构 / Analyze documentation structure
    analyze_documentation
    
    # 分析环境配置 / Analyze environment configuration
    analyze_environment
    
    # 分析数据管理 / Analyze data management
    analyze_data_management
    
    # 分析代码质量 / Analyze code quality
    analyze_code_quality
    
    # 生成统计报告 / Generate statistics report
    generate_statistics_report
    
    # 保存统计结果 / Save statistics results
    save_statistics
    
    log_success "项目统计报告生成完成！/ Project statistics report generated successfully!"
    log_info "详细统计结果已保存到: results/project_statistics.json"
}

# 错误处理 / Error handling
trap 'log_error "项目统计报告生成失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
