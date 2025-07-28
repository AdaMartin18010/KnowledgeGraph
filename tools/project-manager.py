#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
知识图谱项目管理工具 / Knowledge Graph Project Management Tool
用于自动化检查文档质量、生成进度报告和管理交叉引用
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from datetime import datetime

@dataclass
class DocumentMetrics:
    """文档质量指标"""
    file_path: str
    word_count: int
    bilingual_ratio: float
    has_formal_proofs: bool
    has_code_examples: bool
    has_critical_analysis: bool
    has_references: bool
    internal_links: List[str]
    external_links: List[str]

class KnowledgeGraphProjectManager:
    """知识图谱项目管理器"""
    
    def __init__(self, project_root: str = "."):
        self.project_root = Path(project_root)
        self.docs_dir = self.project_root / "docs"
        self.tools_dir = self.project_root / "tools"
        self.reports_dir = self.project_root / "reports"
        
        # 创建必要的目录
        self.reports_dir.mkdir(exist_ok=True)
        
    def scan_documents(self) -> List[DocumentMetrics]:
        """扫描所有文档并收集指标"""
        metrics = []
        
        for doc_file in self.docs_dir.rglob("*.md"):
            if doc_file.name == "template.md":
                continue
                
            metrics.append(self._analyze_document(doc_file))
            
        return metrics
    
    def _analyze_document(self, file_path: Path) -> DocumentMetrics:
        """分析单个文档"""
        content = file_path.read_text(encoding='utf-8')
        
        # 字数统计
        word_count = len(content.split())
        
        # 双语比例
        chinese_chars = len(re.findall(r'[\u4e00-\u9fff]', content))
        english_chars = len(re.findall(r'[a-zA-Z]', content))
        bilingual_ratio = min(chinese_chars, english_chars) / max(chinese_chars, english_chars) if max(chinese_chars, english_chars) > 0 else 0
        
        # 检查各种要素
        has_formal_proofs = bool(re.search(r'定理|Theorem|证明|Proof', content))
        has_code_examples = bool(re.search(r'```(rust|haskell|lean)', content))
        has_critical_analysis = bool(re.search(r'批判性分析|Critical Analysis', content))
        has_references = bool(re.search(r'参考文献|References', content))
        
        # 链接提取
        internal_links = re.findall(r'\[([^\]]+)\]\(\.\./[^)]+\)', content)
        external_links = re.findall(r'\[([^\]]+)\]\(https?://[^)]+\)', content)
        
        return DocumentMetrics(
            file_path=str(file_path.relative_to(self.project_root)),
            word_count=word_count,
            bilingual_ratio=bilingual_ratio,
            has_formal_proofs=has_formal_proofs,
            has_code_examples=has_code_examples,
            has_critical_analysis=has_critical_analysis,
            has_references=has_references,
            internal_links=internal_links,
            external_links=external_links
        )
    
    def generate_progress_report(self) -> Dict:
        """生成项目进度报告"""
        metrics = self.scan_documents()
        
        total_docs = len(metrics)
        completed_docs = sum(1 for m in metrics if m.word_count > 1000)
        
        # 计算质量指标
        avg_bilingual_ratio = sum(m.bilingual_ratio for m in metrics) / len(metrics) if metrics else 0
        docs_with_proofs = sum(1 for m in metrics if m.has_formal_proofs)
        docs_with_code = sum(1 for m in metrics if m.has_code_examples)
        docs_with_analysis = sum(1 for m in metrics if m.has_critical_analysis)
        docs_with_refs = sum(1 for m in metrics if m.has_references)
        
        report = {
            "timestamp": datetime.now().isoformat(),
            "project_status": {
                "total_documents": total_docs,
                "completed_documents": completed_docs,
                "completion_rate": completed_docs / total_docs if total_docs > 0 else 0
            },
            "quality_metrics": {
                "average_bilingual_ratio": avg_bilingual_ratio,
                "documents_with_formal_proofs": docs_with_proofs,
                "documents_with_code_examples": docs_with_code,
                "documents_with_critical_analysis": docs_with_analysis,
                "documents_with_references": docs_with_refs
            },
            "document_details": [
                {
                    "file": m.file_path,
                    "word_count": m.word_count,
                    "bilingual_ratio": m.bilingual_ratio,
                    "has_formal_proofs": m.has_formal_proofs,
                    "has_code_examples": m.has_code_examples,
                    "has_critical_analysis": m.has_critical_analysis,
                    "has_references": m.has_references,
                    "internal_links": m.internal_links,
                    "external_links": m.external_links
                }
                for m in metrics
            ]
        }
        
        return report
    
    def save_report(self, report: Dict, filename: str = None):
        """保存报告到文件"""
        if filename is None:
            filename = f"progress_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        report_path = self.reports_dir / filename
        with open(report_path, 'w', encoding='utf-8') as f:
            json.dump(report, f, ensure_ascii=False, indent=2)
        
        print(f"报告已保存到: {report_path}")
    
    def generate_markdown_report(self, report: Dict) -> str:
        """生成Markdown格式的报告"""
        md_content = f"""# 知识图谱项目进度报告 / Knowledge Graph Project Progress Report

## 生成时间 / Generated Time
{report['timestamp']}

## 项目状态 / Project Status

| 指标 / Metric | 数值 / Value |
|--------------|-------------|
| 总文档数 / Total Documents | {report['project_status']['total_documents']} |
| 已完成文档 / Completed Documents | {report['project_status']['completed_documents']} |
| 完成率 / Completion Rate | {report['project_status']['completion_rate']:.2%} |

## 质量指标 / Quality Metrics

| 指标 / Metric | 数值 / Value |
|--------------|-------------|
| 平均双语比例 / Average Bilingual Ratio | {report['quality_metrics']['average_bilingual_ratio']:.2%} |
| 包含形式化证明的文档 / Documents with Formal Proofs | {report['quality_metrics']['documents_with_formal_proofs']} |
| 包含代码示例的文档 / Documents with Code Examples | {report['quality_metrics']['documents_with_code_examples']} |
| 包含批判性分析的文档 / Documents with Critical Analysis | {report['quality_metrics']['documents_with_critical_analysis']} |
| 包含参考文献的文档 / Documents with References | {report['quality_metrics']['documents_with_references']} |

## 文档详情 / Document Details

"""
        
        for doc in report['document_details']:
            md_content += f"""### {doc['file']}

- **字数** / Word Count: {doc['word_count']}
- **双语比例** / Bilingual Ratio: {doc['bilingual_ratio']:.2%}
- **形式化证明** / Formal Proofs: {'✓' if doc['has_formal_proofs'] else '✗'}
- **代码示例** / Code Examples: {'✓' if doc['has_code_examples'] else '✗'}
- **批判性分析** / Critical Analysis: {'✓' if doc['has_critical_analysis'] else '✗'}
- **参考文献** / References: {'✓' if doc['has_references'] else '✗'}
- **内部链接** / Internal Links: {len(doc['internal_links'])}
- **外部链接** / External Links: {len(doc['external_links'])}

"""
        
        return md_content
    
    def check_consistency(self) -> Dict:
        """检查项目一致性"""
        issues = []
        
        # 检查目录结构
        expected_dirs = [
            "01-knowledge-representation",
            "02-graph-theory", 
            "03-semantic-analysis",
            "04-ontology-engineering",
            "05-knowledge-extraction",
            "06-reasoning-systems",
            "07-applications",
            "08-formal-methods",
            "09-engineering-practices",
            "10-research-methodology"
        ]
        
        for dir_name in expected_dirs:
            dir_path = self.docs_dir / dir_name
            if not dir_path.exists():
                issues.append(f"缺少目录: {dir_name}")
            elif not (dir_path / "README.md").exists():
                issues.append(f"缺少README文件: {dir_name}/README.md")
        
        # 检查交叉引用
        metrics = self.scan_documents()
        for metric in metrics:
            for link in metric.internal_links:
                # 检查内部链接是否有效
                if not self._check_internal_link(link):
                    issues.append(f"无效的内部链接: {metric.file_path} -> {link}")
        
        return {
            "issues": issues,
            "issue_count": len(issues)
        }
    
    def _check_internal_link(self, link_text: str) -> bool:
        """检查内部链接是否有效"""
        # 简化的链接检查逻辑
        return any(link_text.lower() in dir_name.lower() 
                  for dir_name in os.listdir(self.docs_dir))
    
    def generate_next_steps(self, report: Dict) -> List[str]:
        """生成下一步行动计划"""
        steps = []
        
        completion_rate = report['project_status']['completion_rate']
        if completion_rate < 0.5:
            steps.append("优先完成核心模块的内容开发")
        
        avg_bilingual = report['quality_metrics']['average_bilingual_ratio']
        if avg_bilingual < 0.8:
            steps.append("加强双语对照的完善")
        
        docs_with_proofs = report['quality_metrics']['documents_with_formal_proofs']
        total_docs = report['project_status']['total_documents']
        if docs_with_proofs < total_docs * 0.8:
            steps.append("增加形式化证明的内容")
        
        consistency_check = self.check_consistency()
        if consistency_check['issue_count'] > 0:
            steps.append("修复项目一致性问题")
        
        return steps

def main():
    """主函数"""
    manager = KnowledgeGraphProjectManager()
    
    print("🔍 扫描项目文档...")
    report = manager.generate_progress_report()
    
    print("📊 生成进度报告...")
    manager.save_report(report)
    
    print("📝 生成Markdown报告...")
    md_report = manager.generate_markdown_report(report)
    report_path = manager.reports_dir / f"progress_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md"
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write(md_report)
    
    print("🔍 检查项目一致性...")
    consistency = manager.check_consistency()
    
    print("📋 生成下一步计划...")
    next_steps = manager.generate_next_steps(report)
    
    # 输出摘要
    print("\n" + "="*50)
    print("📈 项目进度摘要 / Project Progress Summary")
    print("="*50)
    print(f"总文档数: {report['project_status']['total_documents']}")
    print(f"完成率: {report['project_status']['completion_rate']:.2%}")
    print(f"平均双语比例: {report['quality_metrics']['average_bilingual_ratio']:.2%}")
    print(f"一致性问题: {consistency['issue_count']} 个")
    
    print("\n📋 下一步行动计划 / Next Action Plan:")
    for i, step in enumerate(next_steps, 1):
        print(f"{i}. {step}")
    
    print(f"\n📄 详细报告已保存到: {manager.reports_dir}")

if __name__ == "__main__":
    main() 