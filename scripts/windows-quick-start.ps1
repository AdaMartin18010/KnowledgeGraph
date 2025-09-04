#!/usr/bin/env pwsh
# Windows快速启动脚本 / Windows Quick Start Script
# 知识图谱项目环境搭建 / Knowledge Graph Project Environment Setup

Write-Host "🚀 知识图谱项目快速启动 / Knowledge Graph Project Quick Start" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan

# 检查Docker环境
Write-Host "🔍 检查Docker环境 / Checking Docker environment..." -ForegroundColor Yellow
try {
    docker --version | Out-Null
    Write-Host "✅ Docker已安装 / Docker is installed" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker未安装，请先安装Docker Desktop / Docker not installed, please install Docker Desktop first" -ForegroundColor Red
    exit 1
}

# 检查Docker服务状态
Write-Host "🔍 检查Docker服务状态 / Checking Docker service status..." -ForegroundColor Yellow
try {
    docker info | Out-Null
    Write-Host "✅ Docker服务运行正常 / Docker service is running" -ForegroundColor Green
} catch {
    Write-Host "❌ Docker服务未启动，请启动Docker Desktop / Docker service not started, please start Docker Desktop" -ForegroundColor Red
    exit 1
}

# 构建基础环境
Write-Host "🏗️  构建基础环境 / Building base environment..." -ForegroundColor Yellow
try {
    Set-Location "env/containers"
    docker build -f dockerfiles/base/Dockerfile -t knowledge-graph-base .
    Write-Host "✅ 基础环境构建成功 / Base environment built successfully" -ForegroundColor Green
} catch {
    Write-Host "❌ 基础环境构建失败 / Base environment build failed" -ForegroundColor Red
    Write-Host "错误信息 / Error: $($_.Exception.Message)" -ForegroundColor Red
}

# 启动评估环境
Write-Host "🚀 启动评估环境 / Starting evaluation environment..." -ForegroundColor Yellow
try {
    docker-compose -f docker-compose/evaluation.yml up -d
    Write-Host "✅ 评估环境启动成功 / Evaluation environment started successfully" -ForegroundColor Green
} catch {
    Write-Host "❌ 评估环境启动失败 / Evaluation environment start failed" -ForegroundColor Red
    Write-Host "错误信息 / Error: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "🎉 环境搭建完成！/ Environment setup completed!" -ForegroundColor Green
Write-Host "📖 请查看docs目录了解详细使用方法 / Please check docs directory for detailed usage" -ForegroundColor Cyan
Write-Host "🔗 项目主页: https://github.com/your-org/knowledge-graph" -ForegroundColor Blue
