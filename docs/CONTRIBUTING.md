# 知识图谱项目贡献指南 / Knowledge Graph Project Contributing Guide

## 🎯 贡献指南概述 / Contributing Guide Overview

欢迎为知识图谱项目做出贡献！本指南为贡献者提供了详细的贡献流程、代码规范、文档标准等信息。无论您是开发者、研究人员还是用户，都可以通过多种方式为项目做出贡献。

## 📋 目录 / Table of Contents

- [知识图谱项目贡献指南 / Knowledge Graph Project Contributing Guide](#知识图谱项目贡献指南--knowledge-graph-project-contributing-guide)
  - [🎯 贡献指南概述 / Contributing Guide Overview](#-贡献指南概述--contributing-guide-overview)
  - [📋 目录 / Table of Contents](#-目录--table-of-contents)
  - [🌟 贡献方式 / Ways to Contribute](#-贡献方式--ways-to-contribute)
    - [代码贡献 / Code Contributions](#代码贡献--code-contributions)
    - [文档贡献 / Documentation Contributions](#文档贡献--documentation-contributions)
    - [测试贡献 / Testing Contributions](#测试贡献--testing-contributions)
    - [设计贡献 / Design Contributions](#设计贡献--design-contributions)
    - [社区贡献 / Community Contributions](#社区贡献--community-contributions)
  - [🛠️ 开发环境设置 / Development Environment Setup](#️-开发环境设置--development-environment-setup)
    - [系统要求 / System Requirements](#系统要求--system-requirements)
    - [环境搭建 / Environment Setup](#环境搭建--environment-setup)
      - [1. 克隆项目 / Clone Project](#1-克隆项目--clone-project)
      - [2. 构建开发环境 / Build Development Environment](#2-构建开发环境--build-development-environment)
      - [3. 启动开发环境 / Start Development Environment](#3-启动开发环境--start-development-environment)
    - [开发工具配置 / Development Tools Configuration](#开发工具配置--development-tools-configuration)
      - [VS Code 配置 / VS Code Configuration](#vs-code-配置--vs-code-configuration)
      - [Git 配置 / Git Configuration](#git-配置--git-configuration)
  - [🔄 贡献流程 / Contributing Process](#-贡献流程--contributing-process)
    - [1. 问题识别 / Issue Identification](#1-问题识别--issue-identification)
    - [2. 分支创建 / Branch Creation](#2-分支创建--branch-creation)
    - [3. 开发工作 / Development Work](#3-开发工作--development-work)
    - [4. 测试验证 / Testing and Validation](#4-测试验证--testing-and-validation)
    - [5. 提交推送 / Commit and Push](#5-提交推送--commit-and-push)
    - [6. 创建拉取请求 / Create Pull Request](#6-创建拉取请求--create-pull-request)
  - [📝 代码规范 / Code Standards](#-代码规范--code-standards)
    - [通用规范 / General Standards](#通用规范--general-standards)
    - [Python 代码规范 / Python Code Standards](#python-代码规范--python-code-standards)
      - [代码风格 / Code Style](#代码风格--code-style)
      - [类型注解 / Type Annotations](#类型注解--type-annotations)
    - [Shell 脚本规范 / Shell Script Standards](#shell-脚本规范--shell-script-standards)
      - [脚本结构 / Script Structure](#脚本结构--script-structure)
      - [最佳实践 / Best Practices](#最佳实践--best-practices)
    - [Markdown 文档规范 / Markdown Documentation Standards](#markdown-文档规范--markdown-documentation-standards)
      - [文档结构 / Document Structure](#文档结构--document-structure)
      - [格式规范 / Formatting Standards](#格式规范--formatting-standards)
  - [📚 文档规范 / Documentation Standards](#-文档规范--documentation-standards)
    - [文档类型 / Document Types](#文档类型--document-types)
      - [技术文档 / Technical Documentation](#技术文档--technical-documentation)
      - [用户文档 / User Documentation](#用户文档--user-documentation)
      - [学术文档 / Academic Documentation](#学术文档--academic-documentation)
    - [文档质量标准 / Documentation Quality Standards](#文档质量标准--documentation-quality-standards)
  - [🧪 测试规范 / Testing Standards](#-测试规范--testing-standards)
    - [测试类型 / Test Types](#测试类型--test-types)
      - [单元测试 / Unit Tests](#单元测试--unit-tests)
      - [集成测试 / Integration Tests](#集成测试--integration-tests)
      - [性能测试 / Performance Tests](#性能测试--performance-tests)
    - [测试覆盖率 / Test Coverage](#测试覆盖率--test-coverage)
  - [📝 提交规范 / Commit Standards](#-提交规范--commit-standards)
    - [提交消息格式 / Commit Message Format](#提交消息格式--commit-message-format)
      - [类型说明 / Type Description](#类型说明--type-description)
      - [提交示例 / Commit Examples](#提交示例--commit-examples)
    - [提交频率 / Commit Frequency](#提交频率--commit-frequency)
  - [🔍 审查流程 / Review Process](#-审查流程--review-process)
    - [审查标准 / Review Standards](#审查标准--review-standards)
      - [代码质量 / Code Quality](#代码质量--code-quality)
      - [文档质量 / Documentation Quality](#文档质量--documentation-quality)
      - [测试覆盖 / Test Coverage](#测试覆盖--test-coverage)
    - [审查流程 / Review Process](#审查流程--review-process)
  - [🚀 发布流程 / Release Process](#-发布流程--release-process)
    - [版本管理 / Version Management](#版本管理--version-management)
    - [发布步骤 / Release Steps](#发布步骤--release-steps)
  - [🤝 社区准则 / Community Guidelines](#-社区准则--community-guidelines)
    - [行为准则 / Code of Conduct](#行为准则--code-of-conduct)
    - [沟通准则 / Communication Guidelines](#沟通准则--communication-guidelines)
    - [冲突解决 / Conflict Resolution](#冲突解决--conflict-resolution)
  - [📞 获取帮助 / Getting Help](#-获取帮助--getting-help)
    - [官方资源 / Official Resources](#官方资源--official-resources)
    - [社区支持 / Community Support](#社区支持--community-support)
    - [联系信息 / Contact Information](#联系信息--contact-information)

## 🌟 贡献方式 / Ways to Contribute

### 代码贡献 / Code Contributions

- **功能开发**: 开发新功能、改进现有功能
- **Bug修复**: 修复代码中的错误和问题
- **性能优化**: 优化代码性能和资源使用
- **测试覆盖**: 增加测试用例和测试覆盖率
- **代码重构**: 改进代码结构和可读性

### 文档贡献 / Documentation Contributions

- **文档编写**: 编写新的文档和教程
- **文档翻译**: 翻译文档到其他语言
- **文档改进**: 改进现有文档的清晰度和准确性
- **示例代码**: 提供示例代码和用例
- **API文档**: 完善API文档和接口说明

### 测试贡献 / Testing Contributions

- **测试用例**: 编写新的测试用例
- **测试框架**: 改进测试框架和工具
- **性能测试**: 进行性能测试和基准测试
- **兼容性测试**: 测试不同环境的兼容性
- **用户测试**: 进行用户体验测试

### 设计贡献 / Design Contributions

- **UI/UX设计**: 改进用户界面和用户体验
- **架构设计**: 参与系统架构设计
- **流程设计**: 设计工作流程和业务流程
- **视觉设计**: 提供视觉设计和品牌元素

### 社区贡献 / Community Contributions

- **问题报告**: 报告Bug和问题
- **功能建议**: 提出新功能建议
- **用户支持**: 帮助其他用户解决问题
- **知识分享**: 分享使用经验和最佳实践
- **推广宣传**: 推广项目到更广泛的社区

## 🛠️ 开发环境设置 / Development Environment Setup

### 系统要求 / System Requirements

- **操作系统**: Windows 10+, macOS 10.15+, Ubuntu 18.04+
- **Docker**: Docker Desktop 20.10+ 或 Docker Engine 20.10+
- **Docker Compose**: 1.29+
- **Git**: 2.20+
- **编辑器**: VS Code, Vim, Emacs 等
- **终端**: PowerShell (Windows), Terminal (macOS), Bash (Linux)

### 环境搭建 / Environment Setup

#### 1. 克隆项目 / Clone Project

```bash
# 克隆主仓库
git clone https://github.com/your-username/KnowledgeGraph.git
cd KnowledgeGraph

# 添加上游仓库
git remote add upstream https://github.com/original-owner/KnowledgeGraph.git
```

#### 2. 构建开发环境 / Build Development Environment

```bash
# 构建基础环境
docker build -f env/containers/dockerfiles/base/Dockerfile \
  -t ghcr.io/kg/base:latest \
  env/containers/dockerfiles/base/

# 构建开发环境
docker build -f env/containers/dockerfiles/knowledge-representation/Dockerfile \
  -t ghcr.io/kg/kr-dev:latest \
  env/containers/dockerfiles/knowledge-representation/
```

#### 3. 启动开发环境 / Start Development Environment

```bash
# 启动开发容器
docker run -it --rm \
  -v $(pwd):/workspace \
  -p 5000:5000 \
  ghcr.io/kg/kr-dev:latest

# 在容器内安装开发依赖
pip install -r requirements-dev.txt
```

### 开发工具配置 / Development Tools Configuration

#### VS Code 配置 / VS Code Configuration

```json
{
  "python.defaultInterpreter": "/usr/bin/python3",
  "python.linting.enabled": true,
  "python.linting.pylintEnabled": true,
  "python.formatting.provider": "black",
  "editor.formatOnSave": true,
  "files.associations": {
    "*.md": "markdown",
    "*.yml": "yaml",
    "*.yaml": "yaml"
  }
}
```

#### Git 配置 / Git Configuration

```bash
# 配置用户信息
git config user.name "Your Name"
git config user.email "your.email@example.com"

# 配置编辑器
git config core.editor "code --wait"

# 配置换行符
git config core.autocrlf input  # Linux/macOS
git config core.autocrlf true   # Windows
```

## 🔄 贡献流程 / Contributing Process

### 1. 问题识别 / Issue Identification

在开始贡献之前，请先检查现有的问题和建议：

```bash
# 查看现有问题
# 访问 GitHub Issues 页面
# 搜索相关的问题和建议
```

### 2. 分支创建 / Branch Creation

为您的贡献创建新的分支：

```bash
# 确保主分支是最新的
git checkout main
git pull upstream main

# 创建新的功能分支
git checkout -b feature/your-feature-name
# 或
git checkout -b bugfix/your-bug-fix
# 或
git checkout -b docs/your-documentation-improvement
```

### 3. 开发工作 / Development Work

在分支上进行开发工作：

```bash
# 进行代码修改
# 编写测试用例
# 更新文档

# 添加修改的文件
git add .

# 提交修改
git commit -m "feat: add new feature description"
```

### 4. 测试验证 / Testing and Validation

确保您的修改通过了所有测试：

```bash
# 运行项目健康检查
bash scripts/health-check.sh

# 运行项目完成度检查
bash scripts/project-completion-check.sh

# 运行文档检查
powershell -ExecutionPolicy Bypass -File docs/tools/docs-check.ps1

# 运行代码测试
python -m pytest tests/
```

### 5. 提交推送 / Commit and Push

将您的修改推送到远程仓库：

```bash
# 推送分支到远程仓库
git push origin feature/your-feature-name
```

### 6. 创建拉取请求 / Create Pull Request

在GitHub上创建拉取请求：

1. 访问项目的GitHub页面
2. 点击"New Pull Request"
3. 选择您的分支
4. 填写PR标题和描述
5. 添加相关的标签和审查者

## 📝 代码规范 / Code Standards

### 通用规范 / General Standards

- **可读性**: 代码应该清晰易读，有意义的变量和函数名
- **一致性**: 遵循项目的代码风格和约定
- **简洁性**: 避免不必要的复杂性，保持代码简洁
- **可维护性**: 代码应该易于理解和修改
- **可测试性**: 代码应该易于测试和验证

### Python 代码规范 / Python Code Standards

#### 代码风格 / Code Style

```python
# 使用 PEP 8 风格指南
# 使用 Black 进行代码格式化
# 使用 isort 进行导入排序

# 示例代码
def calculate_knowledge_graph_metrics(graph_data: dict) -> dict:
    """
    计算知识图谱的评估指标。
    
    Args:
        graph_data: 图数据字典
        
    Returns:
        包含评估指标的字典
    """
    metrics = {}
    
    # 计算节点数量
    metrics['node_count'] = len(graph_data.get('nodes', []))
    
    # 计算边数量
    metrics['edge_count'] = len(graph_data.get('edges', []))
    
    # 计算密度
    if metrics['node_count'] > 1:
        max_edges = metrics['node_count'] * (metrics['node_count'] - 1)
        metrics['density'] = metrics['edge_count'] / max_edges
    else:
        metrics['density'] = 0.0
    
    return metrics
```

#### 类型注解 / Type Annotations

```python
from typing import Dict, List, Optional, Union
import numpy as np

def process_graph_data(
    nodes: List[Dict[str, Union[str, int, float]]],
    edges: List[Dict[str, Union[str, int, float]]],
    weights: Optional[np.ndarray] = None
) -> Dict[str, Union[int, float, np.ndarray]]:
    """处理图数据并返回处理结果。"""
    pass
```

### Shell 脚本规范 / Shell Script Standards

#### 脚本结构 / Script Structure

```bash
#!/bin/bash

# 知识图谱项目脚本模板 / Knowledge Graph Project Script Template
# 作者 / Author: Your Name
# 版本 / Version: 1.0.0
# 描述 / Description: 脚本功能描述

set -euo pipefail

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

# 主函数 / Main function
main() {
    log_info "开始执行脚本 / Starting script execution"
    
    # 主要逻辑 / Main logic
    log_success "脚本执行完成 / Script execution completed"
}

# 错误处理 / Error handling
trap 'log_error "脚本执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
```

#### 最佳实践 / Best Practices

- 使用 `set -euo pipefail` 确保脚本的健壮性
- 为所有函数添加注释说明
- 使用有意义的变量名
- 实现适当的错误处理
- 添加日志输出便于调试

### Markdown 文档规范 / Markdown Documentation Standards

#### 文档结构 / Document Structure

```markdown
# 文档标题 / Document Title

## 概述 / Overview

文档的简要描述和目的。

## 详细内容 / Detailed Content

### 子章节 / Subsection

具体的内容描述。

## 总结 / Summary

文档的总结和要点。

---

**最后更新** / Last Updated: YYYY-MM-DD  
**版本** / Version: X.Y.Z  
**维护者** / Maintainer: Your Name  
**许可证** / License: [LICENSE](LICENSE)
```

#### 格式规范 / Formatting Standards

- 使用标准的Markdown语法
- 保持一致的标题层级结构
- 使用表格展示结构化信息
- 添加适当的代码块和语法高亮
- 使用链接引用相关文档

## 📚 文档规范 / Documentation Standards

### 文档类型 / Document Types

#### 技术文档 / Technical Documentation

- **API文档**: 详细的接口说明和示例
- **架构文档**: 系统架构和设计说明
- **部署文档**: 部署和配置指南
- **开发文档**: 开发环境设置和开发指南

#### 用户文档 / User Documentation

- **使用指南**: 详细的使用说明和教程
- **快速开始**: 快速上手指南
- **最佳实践**: 使用最佳实践和技巧
- **故障排除**: 常见问题和解决方案

#### 学术文档 / Academic Documentation

- **理论说明**: 理论基础和概念解释
- **方法学**: 研究方法和实验设计
- **评估标准**: 评估指标和基准
- **参考文献**: 相关研究和文献

### 文档质量标准 / Documentation Quality Standards

- **准确性**: 信息准确无误，与代码实现一致
- **完整性**: 覆盖所有必要的功能和特性
- **清晰性**: 表达清晰，易于理解
- **一致性**: 术语和格式保持一致
- **时效性**: 及时更新，反映最新状态

## 🧪 测试规范 / Testing Standards

### 测试类型 / Test Types

#### 单元测试 / Unit Tests

```python
import pytest
from your_module import your_function

def test_your_function():
    """测试your_function的基本功能。"""
    # 准备测试数据
    input_data = "test_input"
    expected_output = "expected_output"
    
    # 执行测试
    actual_output = your_function(input_data)
    
    # 验证结果
    assert actual_output == expected_output

def test_your_function_edge_cases():
    """测试your_function的边界情况。"""
    # 测试空输入
    assert your_function("") == ""
    
    # 测试None输入
    with pytest.raises(ValueError):
        your_function(None)
```

#### 集成测试 / Integration Tests

```python
def test_knowledge_graph_pipeline():
    """测试知识图谱处理管道的集成功能。"""
    # 准备测试数据
    test_data = load_test_data()
    
    # 执行完整流程
    result = run_knowledge_graph_pipeline(test_data)
    
    # 验证结果
    assert result is not None
    assert 'nodes' in result
    assert 'edges' in result
    assert len(result['nodes']) > 0
```

#### 性能测试 / Performance Tests

```python
import time
import pytest

def test_performance():
    """测试函数的性能表现。"""
    start_time = time.time()
    
    # 执行性能测试
    result = perform_expensive_operation()
    
    end_time = time.time()
    execution_time = end_time - start_time
    
    # 验证性能要求
    assert execution_time < 1.0  # 执行时间应小于1秒
    assert result is not None
```

### 测试覆盖率 / Test Coverage

- **目标覆盖率**: 至少80%的代码覆盖率
- **关键路径**: 100%覆盖关键业务逻辑
- **边界情况**: 覆盖各种边界条件和异常情况
- **集成测试**: 覆盖主要的功能集成点

## 📝 提交规范 / Commit Standards

### 提交消息格式 / Commit Message Format

使用约定式提交格式：

```text
<类型>[可选的作用域]: <描述>

[可选的正文]

[可选的页脚]
```

#### 类型说明 / Type Description

- **feat**: 新功能
- **fix**: Bug修复
- **docs**: 文档更新
- **style**: 代码格式调整
- **refactor**: 代码重构
- **test**: 测试相关
- **chore**: 构建过程或辅助工具的变动

#### 提交示例 / Commit Examples

```bash
# 新功能
git commit -m "feat: add knowledge graph evaluation metrics"

# Bug修复
git commit -m "fix: resolve memory leak in graph processing"

# 文档更新
git commit -m "docs: update user guide with new features"

# 代码重构
git commit -m "refactor: improve graph algorithm performance"

# 测试相关
git commit -m "test: add comprehensive test coverage for graph module"
```

### 提交频率 / Commit Frequency

- **小步提交**: 每个提交应该是一个小的、完整的功能单元
- **及时提交**: 完成一个小功能后及时提交
- **清晰描述**: 每个提交都有清晰的描述和目的
- **可回滚**: 每个提交都应该可以独立回滚

## 🔍 审查流程 / Review Process

### 审查标准 / Review Standards

#### 代码质量 / Code Quality

- **功能正确性**: 代码实现了预期的功能
- **代码风格**: 遵循项目的代码风格规范
- **性能考虑**: 考虑了性能和资源使用
- **安全性**: 没有安全漏洞和风险

#### 文档质量 / Documentation Quality

- **文档完整性**: 相关的文档都已更新
- **文档准确性**: 文档与代码实现一致
- **文档清晰性**: 文档易于理解和使用

#### 测试覆盖 / Test Coverage

- **测试完整性**: 新功能有相应的测试用例
- **测试质量**: 测试用例覆盖了主要场景
- **测试可维护性**: 测试代码易于维护和扩展

### 审查流程 / Review Process

1. **自动检查**: CI/CD系统自动运行检查
2. **代码审查**: 至少一名维护者进行代码审查
3. **测试验证**: 确保所有测试通过
4. **文档审查**: 检查相关文档的更新
5. **最终批准**: 维护者批准后合并

## 🚀 发布流程 / Release Process

### 版本管理 / Version Management

使用语义化版本控制：

```text
主版本号.次版本号.修订号
MAJOR.MINOR.PATCH
```

- **主版本号**: 不兼容的API修改
- **次版本号**: 向下兼容的功能性新增
- **修订号**: 向下兼容的问题修正

### 发布步骤 / Release Steps

1. **版本准备**: 确定新版本号和发布内容
2. **代码冻结**: 冻结代码，只允许Bug修复
3. **测试验证**: 进行全面测试和验证
4. **文档更新**: 更新版本说明和文档
5. **标签创建**: 创建版本标签
6. **发布公告**: 发布版本公告和说明

## 🤝 社区准则 / Community Guidelines

### 行为准则 / Code of Conduct

- **尊重他人**: 尊重所有社区成员
- **建设性反馈**: 提供建设性的反馈和建议
- **包容性**: 欢迎不同背景和经验的贡献者
- **专业性**: 保持专业和礼貌的交流

### 沟通准则 / Communication Guidelines

- **清晰表达**: 清晰、准确地表达想法和问题
- **积极倾听**: 积极倾听他人的意见和建议
- **建设性讨论**: 参与建设性的讨论和辩论
- **及时响应**: 及时响应他人的问题和请求

### 冲突解决 / Conflict Resolution

- **直接沟通**: 直接与相关人员沟通解决问题
- **寻求帮助**: 在需要时寻求维护者的帮助
- **保持冷静**: 在冲突中保持冷静和理性
- **寻求共识**: 努力寻求共识和解决方案

## 📞 获取帮助 / Getting Help

### 官方资源 / Official Resources

- **项目文档**: [docs/](docs/)
- **贡献指南**: [CONTRIBUTING.md](CONTRIBUTING.md)
- **使用指南**: [USER_GUIDE.md](USER_GUIDE.md)
- **项目索引**: [KNOWLEDGE_GRAPH_INDEX.md](KNOWLEDGE_GRAPH_INDEX.md)

### 社区支持 / Community Support

- **GitHub Issues**: 报告问题和Bug
- **GitHub Discussions**: 讨论功能和改进
- **Pull Requests**: 提交代码贡献
- **Wiki**: 查看项目Wiki和FAQ

### 联系信息 / Contact Information

- **项目维护者**: KnowledgeGraph Team
- **技术支持**: 通过GitHub Issues获取支持
- **合作咨询**: 欢迎学术和工业界的合作咨询

---

**最后更新** / Last Updated: 2025-01-01  
**版本** / Version: v1.0.0  
**维护者** / Maintainer: KnowledgeGraph Team  
**许可证** / License: [LICENSE](../../LICENSE)
