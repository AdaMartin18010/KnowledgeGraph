# 数据快照说明 / Data Snapshots Guide

- 存放各评测报告使用的数据集快照校验文件（.sha256）。
- 命名：`<dataset>.sha256`，内容为标准sha256sum输出：`<HASH>  <FILENAME>`。
- 生成示例（Linux/macOS）：
  - `shasum -a 256 <file> > data/snapshots/<dataset>.sha256`
- 生成示例（Windows PowerShell）：
  - `(Get-FileHash <file> -Algorithm SHA256).Hash + '  ' + '<file>' | Out-File data/snapshots/<dataset>.sha256 -Encoding ascii`

占位文件将被真实校验和替换。
