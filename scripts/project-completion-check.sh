#!/bin/bash

# 知识图谱项目完成度检查脚本 / Knowledge Graph Project Completion Check Script
# 作者 / Author: KnowledgeGraph Team
# 版本 / Version: 1.0.0

set -e

# 颜色定义 / Color Definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
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

# 检查计数器 / Check Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
WARNING_CHECKS=0

# 检查函数 / Check Functions
check_item() {
    local description="$1"
    local condition="$2"
    local severity="${3:-INFO}"  # INFO, WARNING, ERROR
    
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    
    if eval "$condition"; then
        case $severity in
            "INFO")
                log_success "✅ $description"
                PASSED_CHECKS=$((PASSED_CHECKS + 1))
                ;;
            "WARNING")
                log_warning "⚠️  $description"
                WARNING_CHECKS=$((WARNING_CHECKS + 1))
                ;;
            "ERROR")
                log_error "❌ $description"
                FAILED_CHECKS=$((FAILED_CHECKS + 1))
                ;;
        esac
    else
        case $severity in
            "INFO")
                log_error "❌ $description"
                FAILED_CHECKS=$((FAILED_CHECKS + 1))
                ;;
            "WARNING")
                log_warning "⚠️  $description"
                WARNING_CHECKS=$((WARNING_CHECKS + 1))
                ;;
            "ERROR")
                log_error "❌ $description"
                FAILED_CHECKS=$((FAILED_CHECKS + 1))
                ;;
        esac
    fi
}

# 检查文档完整性 / Check Documentation Completeness
check_documentation() {
    log_step "检查文档完整性 / Checking Documentation Completeness"
    
    # 检查核心文档 / Check core documentation
    check_item "项目主README文件存在" "[ -f 'README.md' ]"
    check_item "项目状态报告存在" "[ -f 'PROJECT_STATUS.md' ]"
    check_item "项目需求说明存在" "[ -f 'ai.md' ]"
    
    # 检查核心模块文档 / Check core module documentation
    for i in {01..10}; do
        case $i in
            01) module_name="knowledge-representation" ;;
            02) module_name="graph-theory" ;;
            03) module_name="semantic-analysis" ;;
            04) module_name="ontology-engineering" ;;
            05) module_name="knowledge-extraction" ;;
            06) module_name="reasoning-systems" ;;
            07) module_name="applications" ;;
            08) module_name="formal-methods" ;;
            09) module_name="engineering-practice" ;;
            10) module_name="research-methodology" ;;
        esac
        
        check_item "模块 $i ($module_name) README存在" "[ -f 'docs/$i-$module_name/README.md' ]"
    done
    
    # 检查评测报告 / Check evaluation reports
    check_item "评测报告模板存在" "[ -f 'docs/evaluation-report-template.md' ]"
    check_item "评测报告目录存在" "[ -d 'docs/evaluation-reports' ]"
    
    # 检查示例评测报告 / Check sample evaluation reports
    for i in {01..10}; do
        case $i in
            01) module_name="knowledge-representation" ;;
            02) module_name="graph-theory" ;;
            03) module_name="semantic-analysis" ;;
            04) module_name="ontology-engineering" ;;
            05) module_name="knowledge-extraction" ;;
            06) module_name="reasoning-systems" ;;
            07) module_name="applications" ;;
            08) module_name="formal-methods" ;;
            09) module_name="engineering-practice" ;;
            10) module_name="research-methodology" ;;
        esac
        
        check_item "模块 $i 示例评测报告存在" "[ -f 'docs/evaluation-reports/$i-$module_name-sample.md' ]"
    done
    
    # 检查标准和工具 / Check standards and tools
    check_item "文档标准文件存在" "[ -f 'docs/DOCUMENTATION_STANDARDS.md' ]"
    check_item "项目索引文件存在" "[ -f 'docs/KNOWLEDGE_GRAPH_INDEX.md' ]"
    check_item "术语词典存在" "[ -f 'docs/terminology-dictionary.md' ]"
    check_item "文档检查工具存在" "[ -f 'docs/tools/docs-check.ps1' ]"
    check_item "快照验证工具存在" "[ -f 'docs/tools/snapshot-verify.ps1' ]"
}

# 检查环境配置 / Check Environment Configuration
check_environment() {
    log_step "检查环境配置 / Checking Environment Configuration"
    
    # 检查容器环境 / Check container environment
    check_item "容器README文件存在" "[ -f 'env/containers/README.md' ]"
    check_item "Docker Compose配置存在" "[ -f 'env/containers/docker-compose/evaluation.yml' ]"
    
    # 检查Dockerfile / Check Dockerfiles
    check_item "基础镜像Dockerfile存在" "[ -f 'env/containers/dockerfiles/base/Dockerfile' ]"
    
    for module in knowledge-representation graph-theory semantic-analysis ontology-engineering knowledge-extraction reasoning-systems applications formal-methods engineering-practice research-methodology; do
        check_item "$module 环境Dockerfile存在" "[ -f 'env/containers/dockerfiles/$module/Dockerfile' ]"
    done
    
    # 检查构建脚本 / Check build scripts
    check_item "Linux构建脚本存在" "[ -f 'env/containers/scripts/build-all.sh' ]"
    check_item "Windows构建脚本存在" "[ -f 'env/containers/scripts/build-all.ps1' ]"
}

# 检查数据管理 / Check Data Management
check_data_management() {
    log_step "检查数据管理 / Checking Data Management"
    
    check_item "数据快照README存在" "[ -f 'data/snapshots/README.md' ]"
    check_item "数据快照目录存在" "[ -d 'data/snapshots' ]"
}

# 检查脚本工具 / Check Scripts and Tools
check_scripts() {
    log_step "检查脚本工具 / Checking Scripts and Tools"
    
    # 检查快速启动脚本 / Check quick start scripts
    check_item "Linux快速启动脚本存在" "[ -f 'scripts/quick-start.sh' ]"
    check_item "Windows快速启动脚本存在" "[ -f 'scripts/quick-start.ps1' ]"
    
    # 检查评测脚本 / Check evaluation scripts
    check_item "知识表示评测脚本存在" "[ -f 'scripts/kr_eval.sh' ]"
    check_item "图论评测脚本存在" "[ -f 'scripts/gt_eval.sh' ]"
    check_item "综合评测脚本存在" "[ -f 'scripts/run-all-evaluations.sh' ]"
    check_item "项目完成度检查脚本存在" "[ -f 'scripts/project-completion-check.sh' ]"
}

# 检查许可证和配置 / Check License and Configuration
check_legal() {
    log_step "检查许可证和配置 / Checking License and Configuration"
    
    check_item "许可证文件存在" "[ -f 'LICENSE' ]"
}

# 检查文件权限 / Check File Permissions
check_permissions() {
    log_step "检查文件权限 / Checking File Permissions"
    
    # 检查脚本可执行权限 / Check script executable permissions
    for script in scripts/*.sh; do
        if [ -f "$script" ]; then
            check_item "$script 具有可执行权限" "[ -x '$script' ]" "WARNING"
        fi
    done
}

# 检查项目结构完整性 / Check Project Structure Integrity
check_structure() {
    log_step "检查项目结构完整性 / Checking Project Structure Integrity"
    
    # 检查主要目录 / Check main directories
    check_item "docs目录存在" "[ -d 'docs' ]"
    check_item "data目录存在" "[ -d 'data' ]"
    check_item "env目录存在" "[ -d 'env' ]"
    check_item "scripts目录存在" "[ -d 'scripts' ]"
    
    # 检查子目录结构 / Check subdirectory structure
    check_item "docs包含10个核心模块" "[ \$(find docs -maxdepth 1 -type d -name '[0-9][0-9]-*' | wc -l) -eq 10 ]"
    check_item "env/containers包含必要子目录" "[ -d 'env/containers/dockerfiles' ] && [ -d 'env/containers/scripts' ]"
}

# 生成完成度报告 / Generate Completion Report
generate_completion_report() {
    log_step "生成项目完成度报告 / Generating Project Completion Report"
    
    local completion_rate=$((PASSED_CHECKS * 100 / TOTAL_CHECKS))
    local status_color
    local status_emoji
    
    if [ $completion_rate -ge 95 ]; then
        status_color=$GREEN
        status_emoji="🟢"
        status_text="优秀 / Excellent"
    elif [ $completion_rate -ge 85 ]; then
        status_color=$YELLOW
        status_emoji="🟡"
        status_text="良好 / Good"
    elif [ $completion_rate -ge 70 ]; then
        status_color=$YELLOW
        status_emoji="🟡"
        status_text="一般 / Fair"
    else
        status_color=$RED
        status_emoji="🔴"
        status_text="需要改进 / Needs Improvement"
    fi
    
    echo ""
    echo "=========================================="
    echo "   📊 项目完成度报告 / Project Completion Report"
    echo "=========================================="
    echo ""
    echo "📈 完成度统计 / Completion Statistics:"
    echo "   总检查项 / Total Checks: $TOTAL_CHECKS"
    echo "   通过检查 / Passed Checks: $PASSED_CHECKS"
    echo "   警告检查 / Warning Checks: $WARNING_CHECKS"
    echo "   失败检查 / Failed Checks: $FAILED_CHECKS"
    echo ""
    echo "🎯 完成度 / Completion Rate: ${status_color}${completion_rate}%${NC} $status_emoji"
    echo "📋 状态 / Status: ${status_color}${status_text}${NC}"
    echo ""
    
    if [ $FAILED_CHECKS -gt 0 ]; then
        echo "❌ 需要修复的问题 / Issues to Fix: $FAILED_CHECKS"
        echo "   请检查上述失败的检查项"
    fi
    
    if [ $WARNING_CHECKS -gt 0 ]; then
        echo "⚠️  需要注意的警告 / Warnings to Note: $WARNING_CHECKS"
        echo "   建议修复这些警告以提高项目质量"
    fi
    
    if [ $FAILED_CHECKS -eq 0 ] && [ $WARNING_CHECKS -eq 0 ]; then
        echo "🎉 恭喜！项目已达到高质量标准！"
        echo "   Congratulations! The project has reached high quality standards!"
    fi
    
    echo ""
    echo "=========================================="
}

# 主函数 / Main function
main() {
    log_info "开始知识图谱项目完成度检查 / Starting Knowledge Graph Project completion check"
    echo ""
    
    # 执行各项检查 / Execute various checks
    check_documentation
    echo ""
    
    check_environment
    echo ""
    
    check_data_management
    echo ""
    
    check_scripts
    echo ""
    
    check_legal
    echo ""
    
    check_permissions
    echo ""
    
    check_structure
    echo ""
    
    # 生成完成度报告 / Generate completion report
    generate_completion_report
    
    # 返回状态码 / Return status code
    if [ $FAILED_CHECKS -eq 0 ]; then
        log_success "项目完成度检查完成！项目状态良好！"
        exit 0
    else
        log_warning "项目完成度检查完成！存在需要修复的问题。"
        exit 1
    fi
}

# 错误处理 / Error handling
trap 'log_error "项目完成度检查执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
