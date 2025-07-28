#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
双语分析工具 / Bilingual Analysis Tool
重新定义双语比例的计算方法，更准确地评估文档的双语程度
"""

import re
import json
from pathlib import Path
from typing import Dict, List, Tuple
from dataclasses import dataclass
from datetime import datetime

@dataclass
class BilingualLine:
    """双语行结构"""
    line_number: int
    chinese_text: str
    english_text: str
    line_type: str
    has_bilingual: bool

class BilingualAnalyzer:
    """双语分析器"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.docs_dir = project_root / "docs"
    
    def analyze_document(self, file_path: Path) -> Dict:
        """分析文档的双语情况"""
        content = file_path.read_text(encoding='utf-8')
        lines = content.split('\n')
        
        bilingual_lines = []
        total_lines = 0
        bilingual_count = 0
        
        for i, line in enumerate(lines, 1):
            line = line.strip()
            if not line:
                continue
                
            total_lines += 1
            
            # 分析行的双语情况
            chinese_text = self._extract_chinese(line)
            english_text = self._extract_english(line)
            
            has_bilingual = bool(chinese_text and english_text)
            if has_bilingual:
                bilingual_count += 1
            
            bilingual_lines.append(BilingualLine(
                line_number=i,
                chinese_text=chinese_text,
                english_text=english_text,
                line_type=self._classify_line(line),
                has_bilingual=has_bilingual
            ))
        
        # 计算双语比例（基于行数而不是字符数）
        bilingual_ratio = bilingual_count / total_lines if total_lines > 0 else 0
        
        # 分析不同类型的双语情况
        section_lines = [l for l in bilingual_lines if l.line_type == "section"]
        table_lines = [l for l in bilingual_lines if l.line_type == "table"]
        code_lines = [l for l in bilingual_lines if l.line_type == "code"]
        text_lines = [l for l in bilingual_lines if l.line_type == "text"]
        
        return {
            'file_path': str(file_path.relative_to(self.project_root)),
            'total_lines': total_lines,
            'bilingual_lines': bilingual_count,
            'bilingual_ratio': bilingual_ratio,
            'section_bilingual_ratio': self._calculate_bilingual_ratio(section_lines),
            'table_bilingual_ratio': self._calculate_bilingual_ratio(table_lines),
            'code_bilingual_ratio': self._calculate_bilingual_ratio(code_lines),
            'text_bilingual_ratio': self._calculate_bilingual_ratio(text_lines),
            'bilingual_lines_detail': [
                {
                    'line_number': l.line_number,
                    'chinese_text': l.chinese_text,
                    'english_text': l.english_text,
                    'line_type': l.line_type,
                    'has_bilingual': l.has_bilingual
                }
                for l in bilingual_lines
            ]
        }
    
    def _extract_chinese(self, text: str) -> str:
        """提取中文文本"""
        chinese_chars = re.findall(r'[\u4e00-\u9fff]+', text)
        return ' '.join(chinese_chars)
    
    def _extract_english(self, text: str) -> str:
        """提取英文文本"""
        # 移除中文和特殊字符，保留英文
        english_text = re.sub(r'[\u4e00-\u9fff]', '', text)
        english_words = re.findall(r'[a-zA-Z]+', english_text)
        return ' '.join(english_words)
    
    def _classify_line(self, line: str) -> str:
        """分类行类型"""
        if line.startswith('#'):
            return "section"
        elif line.startswith('|'):
            return "table"
        elif line.startswith('```'):
            return "code"
        elif line.startswith('-') or line.startswith('*'):
            return "list"
        else:
            return "text"
    
    def _calculate_bilingual_ratio(self, lines: List[BilingualLine]) -> float:
        """计算双语比例"""
        if not lines:
            return 0.0
        
        bilingual_count = sum(1 for line in lines if line.has_bilingual)
        return bilingual_count / len(lines)
    
    def generate_report(self) -> Dict:
        """生成双语分析报告"""
        docs_files = list(self.docs_dir.rglob("*.md"))
        
        results = []
        total_bilingual_ratio = 0
        
        for file_path in docs_files:
            if file_path.name == "template.md":
                continue
                
            result = self.analyze_document(file_path)
            results.append(result)
            total_bilingual_ratio += result['bilingual_ratio']
        
        avg_bilingual_ratio = total_bilingual_ratio / len(results) if results else 0
        
        return {
            'timestamp': datetime.now().isoformat(),
            'total_files': len(results),
            'average_bilingual_ratio': avg_bilingual_ratio,
            'results': results
        }

def main():
    """主函数"""
    project_root = Path.cwd()
    analyzer = BilingualAnalyzer(project_root)
    
    print("🔍 分析双语情况...")
    report = analyzer.generate_report()
    
    print(f"📊 双语分析报告 / Bilingual Analysis Report")
    print(f"总文件数: {report['total_files']}")
    print(f"平均双语比例: {report['average_bilingual_ratio']:.2%}")
    
    print("\n📋 详细结果:")
    for result in report['results']:
        print(f"- {result['file_path']}: {result['bilingual_ratio']:.2%}")
        print(f"  总行数: {result['total_lines']}, 双语行数: {result['bilingual_lines']}")
        print(f"  段落双语比例: {result['section_bilingual_ratio']:.2%}")
        print(f"  表格双语比例: {result['table_bilingual_ratio']:.2%}")
        print(f"  代码双语比例: {result['code_bilingual_ratio']:.2%}")
        print(f"  文本双语比例: {result['text_bilingual_ratio']:.2%}")
    
    # 保存报告
    report_file = project_root / "reports" / f"bilingual_analysis_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    report_file.parent.mkdir(exist_ok=True)
    
    with open(report_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, ensure_ascii=False, indent=2)
    
    print(f"\n📄 详细报告已保存到: {report_file}")

if __name__ == "__main__":
    main() 