#!/bin/bash

# 知识图谱项目综合评测脚本 / Knowledge Graph Project Comprehensive Evaluation Script
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

# 检查环境 / Check Environment
check_environment() {
    log_info "检查综合评测环境 / Checking comprehensive evaluation environment..."
    
    # 检查Python环境 / Check Python environment
    if ! command -v python &> /dev/null; then
        log_error "Python未安装或不在PATH中"
        exit 1
    fi
    
    # 检查Docker环境 / Check Docker environment
    if ! command -v docker &> /dev/null; then
        log_warning "Docker未安装，将跳过容器环境评测"
        DOCKER_AVAILABLE=false
    else
        if docker info &> /dev/null; then
            log_success "Docker环境可用 / Docker environment available"
            DOCKER_AVAILABLE=true
        else
            log_warning "Docker未运行，将跳过容器环境评测"
            DOCKER_AVAILABLE=false
        fi
    fi
    
    # 创建结果目录 / Create results directory
    mkdir -p results
    log_success "环境检查完成 / Environment check completed"
}

# 运行知识表示评测 / Run Knowledge Representation Evaluation
run_kr_evaluation() {
    log_step "运行知识表示模块评测 / Running Knowledge Representation module evaluation"
    
    if [ -f "scripts/kr_eval.sh" ]; then
        bash scripts/kr_eval.sh
        log_success "知识表示评测完成 / Knowledge Representation evaluation completed"
    else
        log_warning "知识表示评测脚本未找到，跳过 / KR evaluation script not found, skipping"
    fi
}

# 运行图论评测 / Run Graph Theory Evaluation
run_gt_evaluation() {
    log_step "运行图论模块评测 / Running Graph Theory module evaluation"
    
    if [ -f "scripts/gt_eval.sh" ]; then
        bash scripts/gt_eval.sh
        log_success "图论评测完成 / Graph Theory evaluation completed"
    else
        log_warning "图论评测脚本未找到，跳过 / GT evaluation script not found, skipping"
    fi
}

# 运行语义分析评测 / Run Semantic Analysis Evaluation
run_sa_evaluation() {
    log_step "运行语义分析模块评测 / Running Semantic Analysis module evaluation"
    
    # 创建语义分析评测脚本 / Create semantic analysis evaluation script
    mkdir -p results/semantic-analysis
    
    python << 'EOF'
import spacy
import nltk
import transformers
import time
import json
from datetime import datetime

print("=== 语义分析模块评测 / Semantic Analysis Module Evaluation ===")
print(f"评测时间 / Evaluation Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
print()

# 1. 环境信息 / Environment Information
print("1. 环境信息 / Environment Information")
try:
    print(f"   spaCy版本 / spaCy Version: {spacy.__version__}")
    print(f"   NLTK版本 / NLTK Version: {nltk.__version__}")
    print(f"   Transformers版本 / Transformers Version: {transformers.__version__}")
except Exception as e:
    print(f"   版本信息获取失败 / Version info failed: {e}")
print()

# 2. spaCy模型测试 / spaCy Model Test
print("2. spaCy模型测试 / spaCy Model Test")
start_time = time.time()
try:
    nlp = spacy.load("en_core_web_sm")
    load_time = time.time() - start_time
    print(f"   ✅ spaCy模型加载成功 / spaCy model loaded successfully")
    print(f"   加载时间 / Loading time: {load_time:.2f}s")
    
    # 文本处理测试 / Text processing test
    text = "Knowledge Graph is an important technology for artificial intelligence."
    doc = nlp(text)
    print(f"   文本处理成功 / Text processing successful")
    print(f"   实体数量 / Entity count: {len(doc.ents)}")
    
except Exception as e:
    print(f"   ❌ spaCy模型测试失败 / spaCy model test failed: {e}")
print()

# 3. NLTK测试 / NLTK Test
print("3. NLTK测试 / NLTK Test")
try:
    from nltk.tokenize import word_tokenize
    from nltk.tag import pos_tag
    
    text = "Knowledge Graph technology enables semantic understanding."
    tokens = word_tokenize(text)
    pos_tags = pos_tag(tokens)
    
    print(f"   ✅ NLTK测试成功 / NLTK test successful")
    print(f"   分词结果 / Tokenization: {tokens}")
    print(f"   词性标注 / POS tagging: {pos_tags}")
    
except Exception as e:
    print(f"   ❌ NLTK测试失败 / NLTK test failed: {e}")
print()

# 4. Transformers测试 / Transformers Test
print("4. Transformers测试 / Transformers Test")
start_time = time.time()
try:
    from transformers import pipeline
    
    classifier = pipeline("sentiment-analysis")
    result = classifier("Knowledge Graph is a great technology!")
    
    tf_time = time.time() - start_time
    print(f"   ✅ Transformers测试成功 / Transformers test successful")
    print(f"   情感分析结果 / Sentiment analysis: {result}")
    print(f"   处理时间 / Processing time: {tf_time:.2f}s")
    
except Exception as e:
    print(f"   ❌ Transformers测试失败 / Transformers test failed: {e}")
print()

# 保存结果 / Save results
results = {
    "evaluation_time": datetime.now().isoformat(),
    "tests": {
        "spacy_test": "PASS" if 'nlp' in locals() else "FAIL",
        "nltk_test": "PASS" if 'tokens' in locals() else "FAIL",
        "transformers_test": "PASS" if 'result' in locals() else "FAIL"
    }
}

with open("results/semantic-analysis/evaluation_results.json", "w", encoding="utf-8") as f:
    json.dump(results, f, ensure_ascii=False, indent=2)

print()
print("=== 评测完成 / Evaluation Completed ===")
print(f"结果已保存到: results/semantic-analysis/evaluation_results.json")
EOF

    log_success "语义分析评测完成 / Semantic Analysis evaluation completed"
}

# 运行本体工程评测 / Run Ontology Engineering Evaluation
run_oe_evaluation() {
    log_step "运行本体工程模块评测 / Running Ontology Engineering module evaluation"
    
    mkdir -p results/ontology-engineering
    
    python << 'EOF'
import rdflib
import owlready2
import time
import json
from datetime import datetime

print("=== 本体工程模块评测 / Ontology Engineering Module Evaluation ===")
print(f"评测时间 / Evaluation Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
print()

# 1. RDF处理测试 / RDF Processing Test
print("1. RDF处理测试 / RDF Processing Test")
start_time = time.time()
try:
    g = rdflib.Graph()
    g.add((rdflib.URIRef("http://example.org/Person"), 
            rdflib.RDF.type, 
            rdflib.OWL.Class))
    g.add((rdflib.URIRef("http://example.org/Person"), 
            rdflib.RDFS.label, 
            rdflib.Literal("Person")))
    
    rdf_time = time.time() - start_time
    print(f"   ✅ RDF处理测试成功 / RDF processing test successful")
    print(f"   三元组数量 / Triple count: {len(g)}")
    print(f"   处理时间 / Processing time: {rdf_time:.4f}s")
    
except Exception as e:
    print(f"   ❌ RDF处理测试失败 / RDF processing test failed: {e}")
print()

# 2. OWL处理测试 / OWL Processing Test
print("2. OWL处理测试 / OWL Processing Test")
start_time = time.time()
try:
    from owlready2 import *
    
    onto = get_ontology("http://test.org/onto.owl")
    with onto:
        class Person(Thing):
            pass
        
        class hasName(DataProperty):
            domain = [Person]
            range = [str]
    
    owl_time = time.time() - start_time
    print(f"   ✅ OWL处理测试成功 / OWL processing test successful")
    print(f"   类数量 / Class count: {len(onto.classes())}")
    print(f"   属性数量 / Property count: {len(onto.data_properties())}")
    print(f"   处理时间 / Processing time: {owl_time:.4f}s")
    
except Exception as e:
    print(f"   ❌ OWL处理测试失败 / OWL processing test failed: {e}")
print()

# 保存结果 / Save results
results = {
    "evaluation_time": datetime.now().isoformat(),
    "tests": {
        "rdf_test": "PASS" if 'g' in locals() else "FAIL",
        "owl_test": "PASS" if 'onto' in locals() else "FAIL"
    }
}

with open("results/ontology-engineering/evaluation_results.json", "w", encoding="utf-8") as f:
    json.dump(results, f, ensure_ascii=False, indent=2)

print()
print("=== 评测完成 / Evaluation Completed ===")
print(f"结果已保存到: results/ontology-engineering/evaluation_results.json")
EOF

    log_success "本体工程评测完成 / Ontology Engineering evaluation completed"
}

# 生成综合报告 / Generate Comprehensive Report
generate_comprehensive_report() {
    log_step "生成综合评测报告 / Generating comprehensive evaluation report"
    
    python << 'EOF'
import json
import os
import glob
from datetime import datetime

print("=== 生成综合评测报告 / Generating Comprehensive Evaluation Report ===")

# 收集所有结果 / Collect all results
results_files = glob.glob("results/*/evaluation_results.json")
comprehensive_results = {
    "report_time": datetime.now().isoformat(),
    "total_modules": len(results_files),
    "modules": {},
    "summary": {
        "total_tests": 0,
        "passed_tests": 0,
        "failed_tests": 0
    }
}

for result_file in results_files:
    module_name = result_file.split('/')[1]
    try:
        with open(result_file, 'r', encoding='utf-8') as f:
            module_results = json.load(f)
        
        comprehensive_results["modules"][module_name] = module_results
        
        # 统计测试结果 / Count test results
        if "tests" in module_results:
            tests = module_results["tests"]
            comprehensive_results["summary"]["total_tests"] += len(tests)
            comprehensive_results["summary"]["passed_tests"] += sum(1 for test in tests.values() if test == "PASS")
            comprehensive_results["summary"]["failed_tests"] += sum(1 for test in tests.values() if test == "FAIL")
    
    except Exception as e:
        print(f"读取 {result_file} 失败: {e}")

# 计算成功率 / Calculate success rate
if comprehensive_results["summary"]["total_tests"] > 0:
    success_rate = (comprehensive_results["summary"]["passed_tests"] / comprehensive_results["summary"]["total_tests"]) * 100
    comprehensive_results["summary"]["success_rate"] = f"{success_rate:.1f}%"
else:
    comprehensive_results["summary"]["success_rate"] = "0%"

# 保存综合报告 / Save comprehensive report
with open("results/comprehensive_evaluation_report.json", "w", encoding="utf-8") as f:
    json.dump(comprehensive_results, f, ensure_ascii=False, indent=2)

# 显示报告摘要 / Display report summary
print(f"\n📊 综合评测报告摘要 / Comprehensive Evaluation Report Summary")
print(f"   评测时间 / Evaluation Time: {comprehensive_results['report_time']}")
print(f"   评测模块数 / Total Modules: {comprehensive_results['total_modules']}")
print(f"   总测试数 / Total Tests: {comprehensive_results['summary']['total_tests']}")
print(f"   通过测试数 / Passed Tests: {comprehensive_results['summary']['passed_tests']}")
print(f"   失败测试数 / Failed Tests: {comprehensive_results['summary']['failed_tests']}")
print(f"   成功率 / Success Rate: {comprehensive_results['summary']['success_rate']}")

print(f"\n✅ 综合报告已保存到: results/comprehensive_evaluation_report.json")
EOF

    log_success "综合评测报告生成完成 / Comprehensive evaluation report generated"
}

# 显示最终结果 / Show Final Results
show_final_results() {
    log_info "综合评测结果 / Comprehensive Evaluation Results:"
    
    if [ -f "results/comprehensive_evaluation_report.json" ]; then
        echo ""
        echo "📊 综合报告摘要 / Comprehensive Report Summary:"
        cat results/comprehensive_evaluation_report.json | python -c "
import json, sys
data = json.load(sys.stdin)
print(f'评测时间: {data[\"report_time\"]}')
print(f'评测模块数: {data[\"total_modules\"]}')
print(f'总测试数: {data[\"summary\"][\"total_tests\"]}')
print(f'通过测试数: {data[\"summary\"][\"passed_tests\"]}')
print(f'失败测试数: {data[\"summary\"][\"failed_tests\"]}')
print(f'成功率: {data[\"summary\"][\"success_rate\"]}')
"
    else
        log_warning "未找到综合报告文件 / Comprehensive report file not found"
    fi
}

# 主函数 / Main function
main() {
    log_info "开始知识图谱项目综合评测 / Starting Knowledge Graph Project comprehensive evaluation"
    echo ""
    
    # 检查环境 / Check environment
    check_environment
    
    # 运行各模块评测 / Run module evaluations
    run_kr_evaluation
    echo ""
    
    run_gt_evaluation
    echo ""
    
    run_sa_evaluation
    echo ""
    
    run_oe_evaluation
    echo ""
    
    # 生成综合报告 / Generate comprehensive report
    generate_comprehensive_report
    echo ""
    
    # 显示最终结果 / Show final results
    show_final_results
    
    log_success "知识图谱项目综合评测完成！/ Knowledge Graph Project comprehensive evaluation completed!"
    log_info "所有结果已保存到 results/ 目录 / All results saved to results/ directory"
}

# 错误处理 / Error handling
trap 'log_error "综合评测执行失败。请检查错误信息。"; exit 1' ERR

# 执行主函数 / Execute main function
main "$@"
