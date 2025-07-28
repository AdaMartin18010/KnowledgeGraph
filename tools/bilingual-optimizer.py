#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
双语优化工具 / Bilingual Optimization Tool
用于提升文档的双语比例，确保中英对照的完整性
"""

import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from datetime import datetime

@dataclass
class BilingualSection:
    """双语段落结构"""
    chinese_text: str
    english_text: str
    section_type: str
    line_number: int

@dataclass
class OptimizationResult:
    """优化结果"""
    file_path: str
    original_ratio: float
    optimized_ratio: float
    improvements: List[str]
    suggestions: List[str]

class BilingualOptimizer:
    """双语优化器"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.docs_dir = project_root / "docs"
        
    def analyze_document(self, file_path: Path) -> Dict:
        """分析文档的双语情况"""
        content = file_path.read_text(encoding='utf-8')
        lines = content.split('\n')
        
        # 统计中英文字符
        chinese_chars = len(re.findall(r'[\u4e00-\u9fff]', content))
        english_chars = len(re.findall(r'[a-zA-Z]', content))
        
        # 计算双语比例
        total_chars = chinese_chars + english_chars
        bilingual_ratio = min(chinese_chars, english_chars) / max(chinese_chars, english_chars) if max(chinese_chars, english_chars) > 0 else 0
        
        # 分析段落结构
        sections = self._extract_sections(lines)
        
        return {
            'file_path': str(file_path.relative_to(self.project_root)),
            'chinese_chars': chinese_chars,
            'english_chars': english_chars,
            'total_chars': total_chars,
            'bilingual_ratio': bilingual_ratio,
            'sections': sections
        }
    
    def _extract_sections(self, lines: List[str]) -> List[BilingualSection]:
        """提取双语段落"""
        sections = []
        current_chinese = []
        current_english = []
        current_type = ""
        line_num = 0
        
        for line in lines:
            line_num += 1
            
            # 检测段落类型
            if line.startswith('##'):
                current_type = "section"
            elif line.startswith('###'):
                current_type = "subsection"
            elif line.startswith('|'):
                current_type = "table"
            elif line.startswith('```'):
                current_type = "code"
            else:
                current_type = "text"
            
            # 分离中英文内容
            chinese_part = re.sub(r'[a-zA-Z\s]*', '', line)
            english_part = re.sub(r'[\u4e00-\u9fff\s]*', '', line)
            
            if chinese_part.strip() and english_part.strip():
                # 双语行
                sections.append(BilingualSection(
                    chinese_text=chinese_part.strip(),
                    english_text=english_part.strip(),
                    section_type=current_type,
                    line_number=line_num
                ))
            elif chinese_part.strip():
                # 纯中文行
                current_chinese.append(line)
            elif english_part.strip():
                # 纯英文行
                current_english.append(line)
        
        return sections
    
    def optimize_document(self, file_path: Path) -> OptimizationResult:
        """优化文档的双语比例"""
        analysis = self.analyze_document(file_path)
        original_ratio = analysis['bilingual_ratio']
        
        content = file_path.read_text(encoding='utf-8')
        optimized_content = self._optimize_content(content)
        
        # 计算优化后的比例
        chinese_chars = len(re.findall(r'[\u4e00-\u9fff]', optimized_content))
        english_chars = len(re.findall(r'[a-zA-Z]', optimized_content))
        optimized_ratio = min(chinese_chars, english_chars) / max(chinese_chars, english_chars) if max(chinese_chars, english_chars) > 0 else 0
        
        improvements = self._generate_improvements(analysis)
        suggestions = self._generate_suggestions(analysis)
        
        return OptimizationResult(
            file_path=str(file_path.relative_to(self.project_root)),
            original_ratio=original_ratio,
            optimized_ratio=optimized_ratio,
            improvements=improvements,
            suggestions=suggestions
        )
    
    def _optimize_content(self, content: str) -> str:
        """优化内容，增加英文翻译"""
        lines = content.split('\n')
        optimized_lines = []
        
        for line in lines:
            # 检查是否已经有英文翻译
            if self._has_english_translation(line):
                optimized_lines.append(line)
            else:
                # 尝试添加英文翻译
                translated_line = self._add_english_translation(line)
                optimized_lines.append(translated_line)
        
        return '\n'.join(optimized_lines)
    
    def _has_english_translation(self, line: str) -> bool:
        """检查行是否已有英文翻译"""
        # 检查是否包含英文
        english_chars = len(re.findall(r'[a-zA-Z]', line))
        chinese_chars = len(re.findall(r'[\u4e00-\u9fff]', line))
        
        return english_chars > 0 and chinese_chars > 0
    
    def _add_english_translation(self, line: str) -> str:
        """为中文行添加英文翻译"""
        if not re.search(r'[\u4e00-\u9fff]', line):
            return line
        
        # 简单的翻译映射
        translations = {
            '概述': 'Overview',
            '定义': 'Definition',
            '概念': 'Concepts',
            '历史发展': 'Historical Development',
            '发展历程': 'Development Timeline',
            '核心特征': 'Core Characteristics',
            '特征': 'Features',
            '中文描述': 'Chinese Description',
            '理论基础': 'Theoretical Foundation',
            '数学基础': 'Mathematical Foundation',
            '形式化定义': 'Formal Definition',
            '数学符号': 'Mathematical Notation',
            '定理': 'Theorem',
            '证明': 'Proof',
            '逻辑框架': 'Logical Framework',
            '逻辑结构': 'Logical Structure',
            '批判性分析': 'Critical Analysis',
            '优势分析': 'Strengths Analysis',
            '优势': 'Strengths',
            '局限性分析': 'Limitations Analysis',
            '局限性': 'Limitations',
            '争议与讨论': 'Controversies and Discussions',
            '争议点': 'Controversies',
            '支持观点': 'Supporting Views',
            '反对观点': 'Opposing Views',
            '中立分析': 'Neutral Analysis',
            '工程实践': 'Engineering Practice',
            '实现方法': 'Implementation Methods',
            '算法设计': 'Algorithm Design',
            '数据结构': 'Data Structures',
            '性能分析': 'Performance Analysis',
            '工程案例': 'Engineering Cases',
            '应用领域': 'Application Domains',
            '主要应用': 'Primary Applications',
            '实际案例': 'Real-world Cases',
            '前沿发展': 'Frontier Development',
            '最新研究': 'Latest Research',
            '发展趋势': 'Development Trends',
            '总结与展望': 'Summary and Prospects',
            '核心要点': 'Key Points',
            '未来展望': 'Future Prospects',
            '参考文献': 'References',
            '学术文献': 'Academic Literature',
            '技术文档': 'Technical Documentation',
            '在线资源': 'Online Resources',
            '相关链接': 'Related Links',
            '内部链接': 'Internal Links',
            '外部链接': 'External Links',
            '最后更新': 'Last Updated',
            '版本': 'Version',
            '维护者': 'Maintainer',
        }
        
        # 尝试翻译
        translated_line = line
        for chinese, english in translations.items():
            if chinese in line:
                # 检查是否已经有英文
                if not re.search(r'[a-zA-Z]', line):
                    translated_line = line.replace(chinese, f"{chinese} / {english}")
                break
        
        return translated_line
    
    def _generate_improvements(self, analysis: Dict) -> List[str]:
        """生成改进建议"""
        improvements = []
        
        if analysis['bilingual_ratio'] < 0.8:
            improvements.append("需要增加英文翻译以提高双语比例")
        
        if analysis['chinese_chars'] > analysis['english_chars'] * 2:
            improvements.append("中文内容过多，需要增加英文内容")
        
        if analysis['english_chars'] > analysis['chinese_chars'] * 2:
            improvements.append("英文内容过多，需要增加中文内容")
        
        return improvements
    
    def _generate_suggestions(self, analysis: Dict) -> List[str]:
        """生成具体建议"""
        suggestions = []
        
        # 分析段落结构
        sections = analysis['sections']
        chinese_only_sections = [s for s in sections if s.chinese_text and not s.english_text]
        english_only_sections = [s for s in sections if s.english_text and not s.chinese_text]
        
        if chinese_only_sections:
            suggestions.append(f"发现 {len(chinese_only_sections)} 个纯中文段落，需要添加英文翻译")
        
        if english_only_sections:
            suggestions.append(f"发现 {len(english_only_sections)} 个纯英文段落，需要添加中文翻译")
        
        return suggestions
    
    def generate_report(self) -> Dict:
        """生成双语优化报告"""
        docs_files = list(self.docs_dir.rglob("*.md"))
        
        results = []
        total_original_ratio = 0
        total_optimized_ratio = 0
        
        for file_path in docs_files:
            if file_path.name == "template.md":
                continue
                
            result = self.optimize_document(file_path)
            results.append(result)
            total_original_ratio += result.original_ratio
            total_optimized_ratio += result.optimized_ratio
        
        avg_original_ratio = total_original_ratio / len(results) if results else 0
        avg_optimized_ratio = total_optimized_ratio / len(results) if results else 0
        
        return {
            'timestamp': datetime.now().isoformat(),
            'total_files': len(results),
            'average_original_ratio': avg_original_ratio,
            'average_optimized_ratio': avg_optimized_ratio,
            'improvement': avg_optimized_ratio - avg_original_ratio,
            'results': [vars(result) for result in results]
        }

def main():
    """主函数"""
    project_root = Path.cwd()
    optimizer = BilingualOptimizer(project_root)
    
    print("🔍 分析双语情况...")
    report = optimizer.generate_report()
    
    print(f"📊 双语优化报告 / Bilingual Optimization Report")
    print(f"总文件数: {report['total_files']}")
    print(f"平均原始双语比例: {report['average_original_ratio']:.2%}")
    print(f"平均优化后双语比例: {report['average_optimized_ratio']:.2%}")
    print(f"改进幅度: {report['improvement']:.2%}")
    
    print("\n📋 详细结果:")
    for result in report['results']:
        print(f"- {result['file_path']}: {result['original_ratio']:.2%} -> {result['optimized_ratio']:.2%}")
        if result['improvements']:
            print(f"  改进: {', '.join(result['improvements'])}")
        if result['suggestions']:
            print(f"  建议: {', '.join(result['suggestions'])}")
    
    # 保存报告
    report_file = project_root / "reports" / f"bilingual_optimization_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    report_file.parent.mkdir(exist_ok=True)
    
    with open(report_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, ensure_ascii=False, indent=2)
    
    print(f"\n📄 详细报告已保存到: {report_file}")

if __name__ == "__main__":
    main() 