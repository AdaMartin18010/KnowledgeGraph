# 构建所有知识图谱评测环境镜像 / Build All Knowledge Graph Evaluation Environment Images
# 作者 / Author: KnowledgeGraph Team
# 版本 / Version: 1.0.0

param(
    [switch]$SkipBase = $false,
    [switch]$Force = $false
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

# 检查Docker是否运行 / Check if Docker is running
function Test-DockerRunning {
    try {
        docker info | Out-Null
        Write-LogSuccess "Docker is running"
        return $true
    }
    catch {
        Write-LogError "Docker is not running. Please start Docker Desktop and try again."
        return $false
    }
}

# 构建基础镜像 / Build base image
function Build-BaseImage {
    if ($SkipBase) {
        Write-LogInfo "Skipping base image build as requested"
        return
    }
    
    Write-LogInfo "Building base image..."
    try {
        docker build -f dockerfiles/base/Dockerfile -t ghcr.io/kg/base:latest dockerfiles/base/
        Write-LogSuccess "Base image built successfully"
    }
    catch {
        Write-LogError "Failed to build base image"
        throw
    }
}

# 构建知识表示环境 / Build Knowledge Representation environment
function Build-KnowledgeRepresentation {
    Write-LogInfo "Building Knowledge Representation environment..."
    try {
        docker build -f dockerfiles/knowledge-representation/Dockerfile -t ghcr.io/kg/kr-eval:1.0.0 dockerfiles/knowledge-representation/
        Write-LogSuccess "Knowledge Representation environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Knowledge Representation environment"
        throw
    }
}

# 构建图论环境 / Build Graph Theory environment
function Build-GraphTheory {
    Write-LogInfo "Building Graph Theory environment..."
    try {
        docker build -f dockerfiles/graph-theory/Dockerfile -t ghcr.io/kg/gt-eval:1.0.0 dockerfiles/graph-theory/
        Write-LogSuccess "Graph Theory environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Graph Theory environment"
        throw
    }
}

# 构建语义分析环境 / Build Semantic Analysis environment
function Build-SemanticAnalysis {
    Write-LogInfo "Building Semantic Analysis environment..."
    try {
        docker build -f dockerfiles/semantic-analysis/Dockerfile -t ghcr.io/kg/sa-eval:1.0.0 dockerfiles/semantic-analysis/
        Write-LogSuccess "Semantic Analysis environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Semantic Analysis environment"
        throw
    }
}

# 构建本体工程环境 / Build Ontology Engineering environment
function Build-OntologyEngineering {
    Write-LogInfo "Building Ontology Engineering environment..."
    try {
        docker build -f dockerfiles/ontology-engineering/Dockerfile -t ghcr.io/kg/oe-eval:1.0.0 dockerfiles/ontology-engineering/
        Write-LogSuccess "Ontology Engineering environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Ontology Engineering environment"
        throw
    }
}

# 构建知识抽取环境 / Build Knowledge Extraction environment
function Build-KnowledgeExtraction {
    Write-LogInfo "Building Knowledge Extraction environment..."
    try {
        docker build -f dockerfiles/knowledge-extraction/Dockerfile -t ghcr.io/kg/ke-eval:1.0.0 dockerfiles/knowledge-extraction/
        Write-LogSuccess "Knowledge Extraction environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Knowledge Extraction environment"
        throw
    }
}

# 构建推理系统环境 / Build Reasoning Systems environment
function Build-ReasoningSystems {
    Write-LogInfo "Building Reasoning Systems environment..."
    try {
        docker build -f dockerfiles/reasoning-systems/Dockerfile -t ghcr.io/kg/rs-eval:1.0.0 dockerfiles/reasoning-systems/
        Write-LogSuccess "Reasoning Systems environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Reasoning Systems environment"
        throw
    }
}

# 构建应用环境 / Build Applications environment
function Build-Applications {
    Write-LogInfo "Building Applications environment..."
    try {
        docker build -f dockerfiles/applications/Dockerfile -t ghcr.io/kg/app-eval:1.0.0 dockerfiles/applications/
        Write-LogSuccess "Applications environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Applications environment"
        throw
    }
}

# 构建形式化方法环境 / Build Formal Methods environment
function Build-FormalMethods {
    Write-LogInfo "Building Formal Methods environment..."
    try {
        docker build -f dockerfiles/formal-methods/Dockerfile -t ghcr.io/kg/fm-eval:1.0.0 dockerfiles/formal-methods/
        Write-LogSuccess "Formal Methods environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Formal Methods environment"
        throw
    }
}

# 构建工程实践环境 / Build Engineering Practice environment
function Build-EngineeringPractice {
    Write-LogInfo "Building Engineering Practice environment..."
    try {
        docker build -f dockerfiles/engineering-practice/Dockerfile -t ghcr.io/kg/ep-eval:1.0.0 dockerfiles/engineering-practice/
        Write-LogSuccess "Engineering Practice environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Engineering Practice environment"
        throw
    }
}

# 构建研究方法论环境 / Build Research Methodology environment
function Build-ResearchMethodology {
    Write-LogInfo "Building Research Methodology environment..."
    try {
        docker build -f dockerfiles/research-methodology/Dockerfile -t ghcr.io/kg/rm-eval:1.0.0 dockerfiles/research-methodology/
        Write-LogSuccess "Research Methodology environment built successfully"
    }
    catch {
        Write-LogError "Failed to build Research Methodology environment"
        throw
    }
}

# 显示镜像列表 / Show image list
function Show-Images {
    Write-LogInfo "Built images:"
    docker images | Select-String "ghcr.io/kg"
}

# 主函数 / Main function
function Main {
    Write-LogInfo "Starting to build all Knowledge Graph evaluation environments..."
    
    # 检查Docker / Check Docker
    if (-not (Test-DockerRunning)) {
        exit 1
    }
    
    # 构建所有镜像 / Build all images
    Build-BaseImage
    Build-KnowledgeRepresentation
    Build-GraphTheory
    Build-SemanticAnalysis
    Build-OntologyEngineering
    Build-KnowledgeExtraction
    Build-ReasoningSystems
    Build-Applications
    Build-FormalMethods
    Build-EngineeringPractice
    Build-ResearchMethodology
    
    # 显示结果 / Show results
    Show-Images
    
    Write-LogSuccess "All environments built successfully!"
    Write-LogInfo "You can now run: docker-compose -f docker-compose/evaluation.yml up -d"
}

# 错误处理 / Error handling
try {
    Main
}
catch {
    Write-LogError "Build failed. Please check the error messages above."
    Write-LogError $_.Exception.Message
    exit 1
}
