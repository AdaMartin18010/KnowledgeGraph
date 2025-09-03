# 环境快照说明 / Environment Snapshots Guide

- 存放各评测报告使用的环境快照清单（.txt）或容器镜像标识。
- 命名：`<module>-eval-<version>.txt`，记录OS/驱动/框架/库版本与镜像标签。
- 生成建议：
  - `docker image inspect <image>:<tag> | Out-File env/containers/<module>-eval-<version>.txt`
  - 或导出`pip freeze`/`conda env export`等关键信息。

占位文件将被真实环境清单替换。
