---
title: 区块链 × 知识图谱 实施指南
description: 去中心化知识管理、可验证溯源与智能合约驱动的知识工作流实现路线，含最小可运行样例与部署要点。
---

> 快速总览 / Quick Overview

- **范围**: 知识上链存证、版本与评测卡签名、链下存储指针、合约治理。
- **标准锚点**: 与 KG 侧 RDF/PROV-O/JSON-LD 对齐，存证字段与评测管线 `benchmarks/` 联动。
- **堆栈**: Solidity/Hardhat、Web3、IPFS/对象存储、事件监听与审计。
- **导航**: 参见 `docs/18-blockchain-kg/README.md`，与 `docs/benchmarks/`、`docs/implementation/ai-kg-fusion-guide.md` 互链。

## 1. 场景与目标

- 去中心化知识发布与版本管理（知识卡、模型卡、评测卡上链）。
- 智能合约约束的知识提交流程与审核激励（token/积分）。
- 链下/链上协同：链下大体量内容存储（IPFS/对象存储），链上存证与指针。

## 2. 合约接口（Solidity/示意）

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract KnowledgeRegistry {
    struct Record { bytes32 hash; string uri; address author; uint256 ts; }
    mapping(bytes32 => Record) public records; // id -> record

    event Registered(bytes32 indexed id, bytes32 hash, string uri, address author);

    function register(bytes32 id, bytes32 hash, string calldata uri) external {
        require(records[id].ts == 0, "exists");
        records[id] = Record(hash, uri, msg.sender, block.timestamp);
        emit Registered(id, hash, uri, msg.sender);
    }
}
```

## 3. 客户端示例（Python/web3）

```python
from web3 import Web3
import hashlib, json

w3 = Web3(Web3.HTTPProvider("http://localhost:8545"))
acct = w3.eth.accounts[0]

def sha256_bytes(b: bytes):
    return hashlib.sha256(b).hexdigest()

def register_knowledge(contract, kid: str, payload: dict, uri: str):
    raw = json.dumps(payload, ensure_ascii=False).encode("utf-8")
    h = sha256_bytes(raw)
    tx = contract.functions.register(Web3.keccak(text=kid), Web3.to_bytes(hexstr=h), uri).build_transaction({
        'from': acct, 'nonce': w3.eth.get_transaction_count(acct)
    })
    signed = w3.eth.account.sign_transaction(tx, private_key="<PRIVATE_KEY>")
    w3.eth.send_raw_transaction(signed.rawTransaction)
```

## 4. 与 KG 的协同

- 将 `triples.ttl`、`embeddings.faiss`、`bench.json` 生成哈希与 URI，写入链上 `KnowledgeRegistry`。
- 推理/生成服务读取链上指针校验版本与一致性。

## 5. 部署与测试

- 本地：Ganache/Hardhat/EthereumJS；测试合约部署、事件监听。
- 生产：以太坊兼容链/联盟链；多签管理与监控告警。

### 命令参考（Hardhat）

```bash
npm init -y && npm i --save-dev hardhat
npx hardhat init --yes
# 配置网络与账户后：
npx hardhat compile
npx hardhat run scripts/deploy.js --network <your-net>
```

## 6. 安全与合规

- 敏感数据不上链；仅存储哈希与资源指针。
- 合约权限最小化；升级与回滚策略；费用成本控制。
