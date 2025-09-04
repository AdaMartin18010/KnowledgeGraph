# 知识图谱项目快速启动脚本 / Knowledge Graph Project Quick Start Script
# 作者 / Author: KnowledgeGraph Team
# 版本 / Version: 1.0.0

param(
    [switch]$SkipBuild = $false,
    [switch]$SkipStart = $false,
    [switch]$SkipSample = $false
)

# 颜色定义 / Color Definitions
$Red = "Red"
$Green = "Green"
$Yellow = "Yellow"
$Blue = "Blue"
$White = "White"

# 日志函数 / Logging Functions
function Write-LogInfo {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor $Blue
}

function Write-LogSuccess {
    param([string]$Message)
    Write-Host "[SUCCESS] $Message" -ForegroundColor $Green
}

function Write-LogWarning {
    param([string]$Message)
    Write-Host "[WARNING] $Message" -ForegroundColor $Yellow
}

function Write-LogError {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor $Red
}

# 检查系统要求 / Check System Requirements
function Test-SystemRequirements {
    Write-LogInfo "检查系统要求 / Checking system requirements..."
    
    # 检查Docker / Check Docker
    try {
        docker --version | Out-Null
        Write-LogSuccess "Docker已安装 / Docker is installed"
    }
    catch {
        Write-LogError "Docker未安装。请先安装Docker Desktop。"
        Write-LogInfo "安装指南: https://docs.docker.com/desktop/install/windows-install/"
        return $false
    }
    
    # 检查Docker Compose / Check Docker Compose
    try {
        docker-compose --version | Out-Null
        Write-LogSuccess "Docker Compose已安装 / Docker Compose is installed"
    }
    catch {
        Write-LogError "Docker Compose未安装。请先安装Docker Compose。"
        Write-LogInfo "安装指南: https://docs.docker.com/compose/install/"
        return $false
    }
    
    # 检查Docker是否运行 / Check if Docker is running
    try {
        docker info | Out-Null
        Write-LogSuccess "Docker正在运行 / Docker is running"
    }
    catch {
        Write-LogError "Docker未运行。请启动Docker Desktop。"
        return $false
    }
    
    Write-LogSuccess "系统要求检查通过 / System requirements check passed"
    return $true
}

# 构建环境 / Build Environment
function Build-Environment {
    Write-LogInfo "开始构建知识图谱评测环境 / Building Knowledge Graph evaluation environment..."
    
    # 构建基础镜像 / Build base image
    Write-LogInfo "构建基础镜像 / Building base image..."
    try {
        docker build -f env/containers/dockerfiles/base/Dockerfile -t ghcr.io/kg/base:latest env/containers/dockerfiles/base/
        Write-LogSuccess "基础镜像构建完成 / Base image built successfully"
    }
    catch {
        Write-LogError "基础镜像构建失败 / Failed to build base image"
        throw
    }
    
    # 构建所有专业环境 / Build all specialized environments
    Write-LogInfo "构建专业环境 / Building specialized environments..."
    try {
        & env/containers/scripts/build-all.ps1
        Write-LogSuccess "专业环境构建完成 / Specialized environments built successfully"
    }
    catch {
        Write-LogError "专业环境构建失败 / Failed to build specialized environments"
        throw
    }
    
    Write-LogSuccess "环境构建完成 / Environment build completed"
}

# 启动评测环境 / Start Evaluation Environment
function Start-Environment {
    Write-LogInfo "启动评测环境 / Starting evaluation environment..."
    
    try {
        # 使用Docker Compose启动 / Start with Docker Compose
        docker-compose -f env/containers/docker-compose/evaluation.yml up -d
        Write-LogSuccess "评测环境启动完成 / Evaluation environment started"
    }
    catch {
        Write-LogError "评测环境启动失败 / Failed to start evaluation environment"
        throw
    }
}

# 显示环境状态 / Show Environment Status
function Show-EnvironmentStatus {
    Write-LogInfo "环境状态 / Environment status:"
    
    # 显示运行中的容器 / Show running containers
    Write-Host "运行中的容器 / Running containers:" -ForegroundColor $White
    docker ps --filter "name=kg-" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    
    Write-Host ""
    Write-Host "容器网络 / Container network:" -ForegroundColor $White
    $network = docker network ls | Select-String "kg-network"
    if ($network) {
        Write-Host $network
    } else {
        Write-Host "网络未创建 / Network not created" -ForegroundColor $Yellow
    }
}

# 运行示例评测 / Run Sample Evaluation
function Run-SampleEvaluation {
    Write-LogInfo "运行示例评测 / Running sample evaluation..."
    
    try {
        # 进入知识表示环境运行示例 / Enter Knowledge Representation environment and run sample
        Write-LogInfo "在知识表示环境中运行示例 / Running sample in Knowledge Representation environment..."
        docker exec -it kg-kr-eval bash -c "
            cd /workspace/docs/evaluation-reports &&
            python -c '
import sys
sys.path.append(\"/workspace\")
print(\"示例评测环境准备就绪 / Sample evaluation environment ready\")
print(\"您可以运行: python scripts/kr_eval.sh\")
'
        "
        Write-LogSuccess "示例评测完成 / Sample evaluation completed"
    }
    catch {
        Write-LogWarning "示例评测运行失败，但环境已准备就绪 / Sample evaluation failed, but environment is ready"
    }
}

# 显示使用说明 / Show Usage Instructions
function Show-UsageInstructions {
    Write-LogInfo "使用说明 / Usage instructions:"
    Write-Host ""
    Write-Host "1. 进入特定环境 / Enter specific environment:" -ForegroundColor $White
    Write-Host "   docker exec -it kg-kr-eval bash    # 知识表示环境" -ForegroundColor $Yellow
    Write-Host "   docker exec -it kg-gt-eval bash    # 图论环境" -ForegroundColor $Yellow
    Write-Host "   docker exec -it kg-sa-eval bash    # 语义分析环境" -ForegroundColor $Yellow
    Write-Host ""
    Write-Host "2. 运行评测脚本 / Run evaluation scripts:" -ForegroundColor $White
    Write-Host "   bash scripts/kr_eval.sh            # 知识表示评测" -ForegroundColor $Yellow
    Write-Host "   bash scripts/gt_eval.sh            # 图论评测" -ForegroundColor $Yellow
    Write-Host ""
    Write-Host "3. 查看文档 / View documentation:" -ForegroundColor $White
    Write-Host "   docs/KNOWLEDGE_GRAPH_INDEX.md      # 项目索引" -ForegroundColor $Yellow
    Write-Host "   docs/evaluation-reports/           # 评测报告" -ForegroundColor $Yellow
    Write-Host ""
    Write-Host "4. 停止环境 / Stop environment:" -ForegroundColor $White
    Write-Host "   docker-compose -f env/containers/docker-compose/evaluation.yml down" -ForegroundColor $Yellow
}

# 主函数 / Main function
function Main {
    Write-LogInfo "欢迎使用知识图谱项目快速启动脚本 / Welcome to Knowledge Graph Project Quick Start Script"
    Write-Host ""
    
    # 检查系统要求 / Check system requirements
    if (-not (Test-SystemRequirements)) {
        exit 1
    }
    
    # 构建环境 / Build environment
    if (-not $SkipBuild) {
        $buildChoice = Read-Host "是否构建评测环境？(y/n) / Build evaluation environment? (y/n)"
        if ($buildChoice -eq 'y' -or $buildChoice -eq 'Y') {
            Build-Environment
        } else {
            Write-LogInfo "跳过环境构建 / Skipping environment build"
        }
    }
    
    # 启动环境 / Start environment
    if (-not $SkipStart) {
        $startChoice = Read-Host "是否启动评测环境？(y/n) / Start evaluation environment? (y/n)"
        if ($startChoice -eq 'y' -or $startChoice -eq 'Y') {
            Start-Environment
            Show-EnvironmentStatus
            
            # 运行示例 / Run sample
            if (-not $SkipSample) {
                $sampleChoice = Read-Host "是否运行示例评测？(y/n) / Run sample evaluation? (y/n)"
                if ($sampleChoice -eq 'y' -or $sampleChoice -eq 'Y') {
                    Run-SampleEvaluation
                }
            }
        }
    }
    
    # 显示使用说明 / Show usage instructions
    Write-Host ""
    Show-UsageInstructions
    
    Write-LogSuccess "快速启动完成！/ Quick start completed!"
    Write-LogInfo "更多信息请查看: docs/KNOWLEDGE_GRAPH_INDEX.md"
}

# 错误处理 / Error handling
try {
    Main
}
catch {
    Write-LogError "脚本执行失败。请检查错误信息。"
    Write-LogError $_.Exception.Message
    exit 1
}
