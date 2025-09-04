#!/bin/bash

# 知识图谱项目健康检查脚本 / Knowledge Graph Project Health Check Script
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

# 健康检查计数器 / Health Check Counters
TOTAL_CHECKS=0
HEALTHY_CHECKS=0
WARNING_CHECKS=0
CRITICAL_CHECKS=0

# 健康检查函数 / Health Check Functions
check_health() {
    local description="$1"
    local condition="$2"
    local severity="${3:-INFO}"  # INFO, WARNING, CRITICAL
    
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    
    if eval "$condition"; then
        case $severity in
            "INFO")
                log_success "✅ $description"
                HEALTHY_CHECKS=$((HEALTHY_CHECKS + 1))
                ;;
            "WARNING")
                log_warning "⚠️  $description"
                WARNING_CHECKS=$((WARNING_CHECKS + 1))
                ;;
            "CRITICAL")
                log_error "🚨 $description"
                CRITICAL_CHECKS=$((CRITICAL_CHECKS + 1))
                ;;
        esac
    else
        case $severity in
            "INFO")
                log_error "❌ $description"
                CRITICAL_CHECKS=$((CRITICAL_CHECKS + 1))
                ;;
            "WARNING")
                log_warning "⚠️  $description"
                WARNING_CHECKS=$((WARNING_CHECKS + 1))
                ;;
            "CRITICAL")
                log_error "🚨 $description"
                CRITICAL_CHECKS=$((CRITICAL_CHECKS + 1))
                ;;
        esac
    fi
}

# 检查文件完整性 / Check File Integrity
check_file_integrity() {
    log_step "检查文件完整性 / Checking File Integrity"
    
    # 检查关键文件是否存在 / Check if critical files exist
    check_health "项目主README文件完整性" "[ -s 'README.md' ]"
    check_health "项目里程碑报告完整性" "[ -s 'PROJECT_MILESTONE_REPORT.md' ]"
    check_health "项目状态报告完整性" "[ -s 'PROJECT_STATUS.md' ]"
    check_health "项目需求说明完整性" "[ -s 'ai.md' ]"
    check_health "许可证文件完整性" "[ -s 'LICENSE' ]"
    
    # 检查文档目录完整性 / Check documentation directory integrity
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
        
        check_health "模块 $i README完整性" "[ -s 'docs/$i-$module_name/README.md' ]"
        check_health "模块 $i 示例评测报告完整性" "[ -s 'docs/evaluation-reports/$i-$module_name-sample.md' ]"
    done
    
    # 检查工具和脚本完整性 / Check tools and scripts integrity
    check_health "文档检查工具完整性" "[ -s 'docs/tools/docs-check.ps1' ]"
    check_health "快照验证工具完整性" "[ -s 'docs/tools/snapshot-verify.ps1' ]"
    check_health "项目完成度检查脚本完整性" "[ -s 'scripts/project-completion-check.sh' ]"
    check_health "综合评测脚本完整性" "[ -s 'scripts/run-all-evaluations.sh' ]"
}

# 检查环境配置健康性 / Check Environment Configuration Health
check_environment_health() {
    log_step "检查环境配置健康性 / Checking Environment Configuration Health"
    
    # 检查Dockerfile完整性 / Check Dockerfile integrity
    check_health "基础镜像Dockerfile完整性" "[ -s 'env/containers/dockerfiles/base/Dockerfile' ]"
    
    for module in knowledge-representation graph-theory semantic-analysis ontology-engineering knowledge-extraction reasoning-systems applications formal-methods engineering-practice research-methodology; do
        check_health "$module 环境Dockerfile完整性" "[ -s 'env/containers/dockerfiles/$module/Dockerfile' ]"
    done
    
    # 检查构建脚本完整性 / Check build script integrity
    check_health "Linux构建脚本完整性" "[ -s 'env/containers/scripts/build-all.sh' ]"
    check_health "Windows构建脚本完整性" "[ -s 'env/containers/scripts/build-all.ps1' ]"
    check_health "Docker Compose配置完整性" "[ -s 'env/containers/docker-compose/evaluation.yml' ]"
    
    # 检查README文件完整性 / Check README file integrity
    check_health "容器环境README完整性" "[ -s 'env/containers/README.md' ]"
    check_health "数据快照README完整性" "[ -s 'data/snapshots/README.md' ]"
}

# 检查脚本健康性 / Check Script Health
check_script_health() {
    log_step "检查脚本健康性 / Checking Script Health"
    
    # 检查脚本语法 / Check script syntax
    for script in scripts/*.sh; do
        if [ -f "$script" ]; then
            if bash -n "$script" 2>/dev/null; then
                check_health "$script 语法正确性" "true"
            else
                check_health "$script 语法正确性" "false" "CRITICAL"
            fi
        fi
    done
    
    # 检查脚本可执行权限 / Check script executable permissions
    for script in scripts/*.sh; do
        if [ -f "$script" ]; then
            if [ -x "$script" ]; then
                check_health "$script 可执行权限" "true"
            else
                check_health "$script 可执行权限" "false" "WARNING"
            fi
        fi
    done
}

# 检查链接健康性 / Check Link Health
check_link_health() {
    log_step "检查链接健康性 / Checking Link Health"
    
    # 检查内部链接 / Check internal links
    local broken_links=0
    local total_links=0
    
    # 简单的链接检查 / Simple link check
    for file in docs/**/*.md; do
        if [ -f "$file" ]; then
            # 检查文件中的链接 / Check links in files
            local file_links=$(grep -o '\[.*\]([^)]*)' "$file" 2>/dev/null | wc -l)
            total_links=$((total_links + file_links))
            
            # 检查链接目标是否存在 / Check if link targets exist
            while IFS= read -r link; do
                local target=$(echo "$link" | sed 's/.*](\([^)]*\))/\1/')
                if [[ "$target" == http* ]]; then
                    # 外部链接，跳过检查 / Skip external links
                    continue
                elif [[ "$target" == */* ]]; then
                    # 相对路径链接 / Relative path links
                    if [ ! -f "$target" ] && [ ! -f "docs/$target" ]; then
                        broken_links=$((broken_links + 1))
                    fi
                fi
            done < <(grep -o '\[.*\]([^)]*)' "$file" 2>/dev/null)
        fi
    done
    
    if [ $broken_links -eq 0 ]; then
        check_health "内部链接完整性" "true"
    else
        check_health "内部链接完整性 (发现 $broken_links 个损坏链接)" "false" "WARNING"
    fi
}

# 检查代码质量 / Check Code Quality
check_code_quality() {
    log_step "检查代码质量 / Checking Code Quality"
    
    # 检查Python代码语法 / Check Python code syntax
    local python_files=$(find . -name "*.py" 2>/dev/null | wc -l)
    if [ $python_files -gt 0 ]; then
        local syntax_errors=0
        for pyfile in $(find . -name "*.py" 2>/dev/null); do
            if ! python -m py_compile "$pyfile" 2>/dev/null; then
                syntax_errors=$((syntax_errors + 1))
            fi
        done
        
        if [ $syntax_errors -eq 0 ]; then
            check_health "Python代码语法正确性" "true"
        else
            check_health "Python代码语法正确性 (发现 $syntax_errors 个语法错误)" "false" "CRITICAL"
        fi
    else
        check_health "Python代码语法正确性" "true"
    fi
    
    # 检查Shell脚本语法 / Check Shell script syntax
    local shell_scripts=$(find . -name "*.sh" 2>/dev/null | wc -l)
    if [ $shell_scripts -gt 0 ]; then
        local syntax_errors=0
        for shfile in $(find . -name "*.sh" 2>/dev/null); do
            if ! bash -n "$shfile" 2>/dev/null; then
                syntax_errors=$((syntax_errors + 1))
            fi
        done
        
        if [ $syntax_errors -eq 0 ]; then
            check_health "Shell脚本语法正确性" "true"
        else
            check_health "Shell脚本语法正确性 (发现 $syntax_errors 个语法错误)" "false" "CRITICAL"
        fi
    fi
}

# 检查项目结构健康性 / Check Project Structure Health
check_structure_health() {
    log_step "检查项目结构健康性 / Checking Project Structure Health"
    
    # 检查目录结构 / Check directory structure
    check_health "核心文档目录存在" "[ -d 'docs' ]"
    check_health "数据管理目录存在" "[ -d 'data' ]"
    check_health "环境配置目录存在" "[ -d 'env' ]"
    check_health "运行脚本目录存在" "[ -d 'scripts' ]"
    
    # 检查子目录结构 / Check subdirectory structure
    check_health "评测报告目录存在" "[ -d 'docs/evaluation-reports' ]"
    check_health "工具脚本目录存在" "[ -d 'docs/tools' ]"
    check_health "Dockerfile目录存在" "[ -d 'env/containers/dockerfiles' ]"
    check_health "构建脚本目录存在" "[ -d 'env/containers/scripts' ]"
    check_health "数据快照目录存在" "[ -d 'data/snapshots' ]"
}

# 生成健康报告 / Generate Health Report
generate_health_report() {
    log_step "生成项目健康报告 / Generating Project Health Report"
    
    local health_score=$((HEALTHY_CHECKS * 100 / TOTAL_CHECKS))
    local status_color
    local status_emoji
    local status_text
    
    if [ $health_score -ge 95 ]; then
        status_color=$GREEN
        status_emoji="🟢"
        status_text="优秀 / Excellent"
    elif [ $health_score -ge 85 ]; then
        status_color=$YELLOW
        status_emoji="🟡"
        status_text="良好 / Good"
    elif [ $health_score -ge 70 ]; then
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
    echo "   🏥 项目健康报告 / Project Health Report"
    echo "=========================================="
    echo ""
    echo "📊 健康检查统计 / Health Check Statistics:"
    echo "   总检查项 / Total Checks: $TOTAL_CHECKS"
    echo "   健康检查 / Healthy Checks: $HEALTHY_CHECKS"
    echo "   警告检查 / Warning Checks: $WARNING_CHECKS"
    echo "   严重问题 / Critical Issues: $CRITICAL_CHECKS"
    echo ""
    echo "🏥 健康评分 / Health Score: ${status_color}${health_score}%${NC} $status_emoji"
    echo "📋 状态 / Status: ${status_color}${status_text}${NC}"
    echo ""
    
    if [ $CRITICAL_CHECKS -gt 0 ]; then
        echo "🚨 严重问题 / Critical Issues: $CRITICAL_CHECKS"
        echo "   请立即修复这些问题以确保项目正常运行"
    fi
    
    if [ $WARNING_CHECKS -gt 0 ]; then
        echo "⚠️  警告问题 / Warning Issues: $WARNING_CHECKS"
        echo "   建议修复这些警告以提高项目质量"
    fi
    
    if [ $CRITICAL_CHECKS -eq 0 ] && [ $WARNING_CHECKS -eq 0 ]; then
        echo "🎉 恭喜！项目健康状况优秀！"
        echo "   Congratulations! The project is in excellent health!"
    fi
    
    echo ""
    echo "=========================================="
}

# 主函数 / Main function
main() {
    log_info "开始知识图谱项目健康检查 / Starting Knowledge Graph Project health check"
    echo ""
    
    # 执行各项健康检查 / Execute various health checks
    check_file_integrity
    echo ""
    
    check_environment_health
    echo ""
    
    check_script_health
    echo ""
    
    check_link_health
    echo ""
    
    check_code_quality
    echo ""
    
    check_structure_health
    echo ""
    
    # 生成健康报告 / Generate health report
    generate_health_report
    
    # 返回状态码 / Return status code
    if [ $CRITICAL_CHECKS -eq 0 ]; then
        log_success "项目健康检查完成！项目健康状况良好！"
        exit 0
    else
        log_warning "项目健康检查完成！发现严重问题需要修复。"
        exit 1
    fi
}

# 错误处理 / Error handling
trap 'log_error "项目健康检查执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
