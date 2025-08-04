#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
序号验证工具 / Numbering Validator Tool
检查文档的序号结构，确保严格的层级序号和连续性
"""

import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from datetime import datetime
from enum import Enum

class HeadingLevel(Enum):
    """标题层级枚举"""
    LEVEL_1 = 1  # ## 1. 标题
    LEVEL_2 = 2  # ### 1.1 标题
    LEVEL_3 = 3  # #### 1.1.1 标题
    LEVEL_4 = 4  # ##### 1.1.1.1 标题

@dataclass
class HeadingInfo:
    """标题信息结构"""
    line_number: int
    level: HeadingLevel
    number: str
    title: str
    full_text: str
    has_bilingual: bool

@dataclass
class NumberingIssue:
    """序号问题结构"""
    line_number: int
    issue_type: str
    description: str
    suggestion: str

class NumberingValidator:
    """序号验证器"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.docs_dir = project_root / "docs"
        
    def validate_document(self, file_path: Path) -> Dict:
        """验证文档的序号结构"""
        content = file_path.read_text(encoding='utf-8')
        lines = content.split('\n')
        
        headings = []
        issues = []
        
        for i, line in enumerate(lines, 1):
            line = line.strip()
            if not line:
                continue
                
            # 检查是否是标题行
            heading_info = self._parse_heading(line, i)
            if heading_info:
                headings.append(heading_info)
                
                # 验证序号格式
                number_issue = self._validate_number_format(heading_info)
                if number_issue:
                    issues.append(number_issue)
        
        # 验证序号连续性
        continuity_issues = self._validate_number_continuity(headings)
        issues.extend(continuity_issues)
        
        # 验证层级结构
        hierarchy_issues = self._validate_hierarchy(headings)
        issues.extend(hierarchy_issues)
        
        # 验证双语格式
        bilingual_issues = self._validate_bilingual_format(headings)
        issues.extend(bilingual_issues)
        
        return {
            'file_path': str(file_path.relative_to(self.project_root)),
            'total_headings': len(headings),
            'total_issues': len(issues),
            'headings': [
                {
                    'line_number': h.line_number,
                    'level': h.level.value,
                    'number': h.number,
                    'title': h.title,
                    'full_text': h.full_text,
                    'has_bilingual': h.has_bilingual
                }
                for h in headings
            ],
            'issues': [
                {
                    'line_number': i.line_number,
                    'issue_type': i.issue_type,
                    'description': i.description,
                    'suggestion': i.suggestion
                }
                for i in issues
            ]
        }
    
    def _parse_heading(self, line: str, line_number: int) -> Optional[HeadingInfo]:
        """解析标题行"""
        # 匹配标题格式: ## 1. 标题 / Title
        pattern = r'^(#{2,5})\s+(\d+(?:\.\d+)*)\.\s+(.+?)(?:\s*/\s*(.+))?$'
        match = re.match(pattern, line)
        
        if not match:
            return None
            
        level_markers, number, title, english_title = match.groups()
        level = len(level_markers)
        
        if level > 4:
            return None
            
        # 验证序号格式
        if not self._is_valid_number_format(number, level):
            return None
            
        return HeadingInfo(
            line_number=line_number,
            level=HeadingLevel(level),
            number=number,
            title=title.strip(),
            full_text=line,
            has_bilingual=bool(english_title)
        )
    
    def _is_valid_number_format(self, number: str, level: int) -> bool:
        """验证序号格式是否正确"""
        parts = number.split('.')
        
        # 检查层级数量
        if len(parts) != level:
            return False
            
        # 检查每个部分是否为数字
        for part in parts:
            if not part.isdigit():
                return False
                
        return True
    
    def _validate_number_format(self, heading: HeadingInfo) -> Optional[NumberingIssue]:
        """验证序号格式"""
        # 检查序号是否为空
        if not heading.number:
            return NumberingIssue(
                line_number=heading.line_number,
                issue_type="missing_number",
                description="标题缺少序号",
                suggestion=f"添加序号，例如: {self._suggest_number(heading.level)}"
            )
        
        # 检查序号格式
        if not self._is_valid_number_format(heading.number, heading.level.value):
            return NumberingIssue(
                line_number=heading.line_number,
                issue_type="invalid_number_format",
                description=f"序号格式错误: {heading.number}",
                suggestion=f"使用正确的格式: {self._suggest_number(heading.level)}"
            )
        
        return None
    
    def _validate_number_continuity(self, headings: List[HeadingInfo]) -> List[NumberingIssue]:
        """验证序号连续性"""
        issues = []
        
        # 按层级分组
        for level in HeadingLevel:
            level_headings = [h for h in headings if h.level == level]
            level_headings.sort(key=lambda x: x.line_number)
            
            expected_number = 1
            for heading in level_headings:
                current_number = int(heading.number.split('.')[-1])
                
                if current_number != expected_number:
                    issues.append(NumberingIssue(
                        line_number=heading.line_number,
                        issue_type="discontinuous_number",
                        description=f"序号不连续: 期望 {expected_number}, 实际 {current_number}",
                        suggestion=f"修改序号为 {expected_number}"
                    ))
                
                expected_number += 1
        
        return issues
    
    def _validate_hierarchy(self, headings: List[HeadingInfo]) -> List[NumberingIssue]:
        """验证层级结构"""
        issues = []
        
        for i, heading in enumerate(headings):
            # 检查是否有父级标题
            if heading.level.value > 1:
                parent_level = heading.level.value - 1
                parent_number = '.'.join(heading.number.split('.')[:-1])
                
                # 查找父级标题
                parent_found = False
                for j in range(i-1, -1, -1):
                    if (headings[j].level.value == parent_level and 
                        headings[j].number == parent_number):
                        parent_found = True
                        break
                
                if not parent_found:
                    issues.append(NumberingIssue(
                        line_number=heading.line_number,
                        issue_type="missing_parent",
                        description=f"缺少父级标题: {parent_number}",
                        suggestion=f"添加父级标题: {parent_number}"
                    ))
        
        return issues
    
    def _validate_bilingual_format(self, headings: List[HeadingInfo]) -> List[NumberingIssue]:
        """验证双语格式"""
        issues = []
        
        for heading in headings:
            if not heading.has_bilingual:
                issues.append(NumberingIssue(
                    line_number=heading.line_number,
                    issue_type="missing_bilingual",
                    description="标题缺少英文对照",
                    suggestion=f"添加英文对照: {heading.title} / English Title"
                ))
        
        return issues
    
    def _suggest_number(self, level: HeadingLevel) -> str:
        """建议序号格式"""
        if level == HeadingLevel.LEVEL_1:
            return "1."
        elif level == HeadingLevel.LEVEL_2:
            return "1.1"
        elif level == HeadingLevel.LEVEL_3:
            return "1.1.1"
        elif level == HeadingLevel.LEVEL_4:
            return "1.1.1.1"
        return "1"
    
    def generate_report(self) -> Dict:
        """生成序号验证报告"""
        all_results = []
        total_issues = 0
        
        # 验证所有文档
        for file_path in self.docs_dir.rglob("*.md"):
            if file_path.name == "template.md":
                continue
                
            result = self.validate_document(file_path)
            all_results.append(result)
            total_issues += result['total_issues']
        
        # 验证项目报告文件
        for file_path in self.project_root.glob("*.md"):
            if file_path.name in ["README.md", "LICENSE", "ai.md"]:
                continue
                
            result = self.validate_document(file_path)
            all_results.append(result)
            total_issues += result['total_issues']
        
        return {
            'timestamp': datetime.now().isoformat(),
            'total_files': len(all_results),
            'total_issues': total_issues,
            'files': all_results,
            'summary': {
                'files_with_issues': len([r for r in all_results if r['total_issues'] > 0]),
                'files_without_issues': len([r for r in all_results if r['total_issues'] == 0]),
                'average_issues_per_file': total_issues / len(all_results) if all_results else 0
            }
        }
    
    def fix_issues(self, file_path: Path) -> Dict:
        """修复文档中的序号问题"""
        content = file_path.read_text(encoding='utf-8')
        lines = content.split('\n')
        
        # 分析当前结构
        headings = []
        for i, line in enumerate(lines):
            line = line.strip()
            if not line:
                continue
                
            heading_info = self._parse_heading(line, i+1)
            if heading_info:
                headings.append((i, heading_info))
        
        # 重新编号
        new_lines = lines.copy()
        level_counters = {1: 0, 2: 0, 3: 0, 4: 0}
        
        for line_idx, heading in headings:
            level = heading.level.value
            level_counters[level] += 1
            
            # 构建新序号
            if level == 1:
                new_number = str(level_counters[1])
            elif level == 2:
                new_number = f"{level_counters[1]}.{level_counters[2]}"
            elif level == 3:
                new_number = f"{level_counters[1]}.{level_counters[2]}.{level_counters[3]}"
            elif level == 4:
                new_number = f"{level_counters[1]}.{level_counters[2]}.{level_counters[3]}.{level_counters[4]}"
            
            # 更新行
            old_line = lines[line_idx]
            new_line = re.sub(r'^(#{2,5})\s+\d+(?:\.\d+)*\.\s+', 
                            rf'\1 {new_number}. ', old_line)
            new_lines[line_idx] = new_line
        
        # 写回文件
        file_path.write_text('\n'.join(new_lines), encoding='utf-8')
        
        return {
            'file_path': str(file_path.relative_to(self.project_root)),
            'fixed_headings': len(headings),
            'status': 'fixed'
        }

def main():
    """主函数"""
    project_root = Path(__file__).parent.parent
    validator = NumberingValidator(project_root)
    
    # 生成报告
    report = validator.generate_report()
    
    # 保存报告
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    report_file = project_root / "reports" / f"numbering_validation_{timestamp}.json"
    report_file.parent.mkdir(exist_ok=True)
    
    with open(report_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, ensure_ascii=False, indent=2)
    
    # 打印摘要
    print(f"序号验证报告已生成: {report_file}")
    print(f"总文件数: {report['total_files']}")
    print(f"总问题数: {report['total_issues']}")
    print(f"有问题文件数: {report['summary']['files_with_issues']}")
    print(f"无问题文件数: {report['summary']['files_without_issues']}")
    
    # 显示问题详情
    if report['total_issues'] > 0:
        print("\n问题详情:")
        for file_result in report['files']:
            if file_result['total_issues'] > 0:
                print(f"\n文件: {file_result['file_path']}")
                print(f"问题数: {file_result['total_issues']}")
                for issue in file_result['issues']:
                    print(f"  行 {issue['line_number']}: {issue['description']}")

if __name__ == "__main__":
    main() 