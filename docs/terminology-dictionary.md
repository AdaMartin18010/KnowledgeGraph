# 知识图谱术语对照表 / Knowledge Graph Terminology Dictionary

## 1. 基础概念 / Basic Concepts

### 1.1 知识图谱核心概念 / Core Knowledge Graph Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 知识图谱 | Knowledge Graph | 一种结构化的语义知识库，用于描述概念及其相互关系 | 语义网络、本体、知识库 |
| Knowledge Graph | Knowledge Graph | A structured semantic knowledge base for describing concepts and their relationships | Semantic Network, Ontology, Knowledge Base |
| 实体 | Entity | 知识图谱中的基本单位，表示现实世界中的对象 | 节点、实例、对象 |
| Entity | Entity | Basic unit in knowledge graph representing real-world objects | Node, Instance, Object |
| 关系 | Relation | 连接两个实体的语义关系 | 边、属性、谓词 |
| Relation | Relation | Relationship connecting two entities | Edge, Property, Predicate |
| 属性 | Attribute | 描述实体特征的键值对 | 特征、性质、描述符 |
| Attribute | Attribute | Key-value pairs describing entity characteristics | Feature, Property, Descriptor |
| 三元组 | Triple | 知识图谱的基本表示单位，形式为(主体,关系,客体) | 陈述、事实、命题 |
| Triple | Triple | Basic representation unit in knowledge graph: (subject, relation, object) | Statement, Fact, Proposition |

### 1.2 知识表示概念 / Knowledge Representation Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 知识表示 | Knowledge Representation | 将人类知识转换为计算机可处理的形式化结构 | 符号表示、语义表示 |
| Knowledge Representation | Knowledge Representation | Converting human knowledge into formal structures processable by computers | Symbolic Representation, Semantic Representation |
| 符号表示 | Symbolic Representation | 使用符号和逻辑规则表示知识的方法 | 逻辑表示、规则表示 |
| Symbolic Representation | Symbolic Representation | Method of representing knowledge using symbols and logical rules | Logical Representation, Rule-based Representation |
| 语义表示 | Semantic Representation | 基于语义相似性和关联性的知识表示方法 | 向量表示、嵌入表示 |
| Semantic Representation | Semantic Representation | Knowledge representation based on semantic similarity and associations | Vector Representation, Embedding Representation |
| 本体 | Ontology | 形式化的概念体系，定义概念、关系和约束 | 概念模型、语义模型 |
| Ontology | Ontology | Formal conceptual system defining concepts, relations, and constraints | Conceptual Model, Semantic Model |
| 概念 | Concept | 知识表示中的抽象类别或类型 | 类、类型、范畴 |
| Concept | Concept | Abstract category or type in knowledge representation | Class, Type, Category |

## 2. 图论基础 / Graph Theory Fundamentals

### 2.1 图的基本概念 / Basic Graph Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 图 | Graph | 由节点和边组成的数学结构 | 网络、图结构 |
| Graph | Graph | Mathematical structure consisting of nodes and edges | Network, Graph Structure |
| 节点 | Node | 图中的基本元素，表示实体或概念 | 顶点、点、实体 |
| Node | Node | Basic element in graph representing entities or concepts | Vertex, Point, Entity |
| 边 | Edge | 连接两个节点的关系 | 弧、连接、关系 |
| Edge | Edge | Relationship connecting two nodes | Arc, Connection, Relation |
| 有向图 | Directed Graph | 边具有方向性的图 | 有向网络、定向图 |
| Directed Graph | Directed Graph | Graph where edges have directionality | Directed Network, Oriented Graph |
| 无向图 | Undirected Graph | 边没有方向性的图 | 无向网络、双向图 |
| Undirected Graph | Undirected Graph | Graph where edges have no directionality | Undirected Network, Bidirectional Graph |
| 加权图 | Weighted Graph | 边具有权重值的图 | 带权图、权重网络 |
| Weighted Graph | Weighted Graph | Graph where edges have weight values | Weighted Network, Weighted Graph |

### 2.2 图算法概念 / Graph Algorithm Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 图遍历 | Graph Traversal | 访问图中所有节点的算法 | 搜索算法、遍历算法 |
| Graph Traversal | Graph Traversal | Algorithm for visiting all nodes in a graph | Search Algorithm, Traversal Algorithm |
| 深度优先搜索 | Depth-First Search (DFS) | 优先访问深层节点的遍历算法 | 深度遍历、递归搜索 |
| Depth-First Search | Depth-First Search (DFS) | Traversal algorithm prioritizing deep nodes | Depth Traversal, Recursive Search |
| 广度优先搜索 | Breadth-First Search (BFS) | 优先访问邻近节点的遍历算法 | 广度遍历、层次搜索 |
| Breadth-First Search | Breadth-First Search (BFS) | Traversal algorithm prioritizing neighboring nodes | Breadth Traversal, Level Search |
| 最短路径 | Shortest Path | 图中两个节点间的最短距离路径 | 最优路径、最小距离 |
| Shortest Path | Shortest Path | Path with minimum distance between two nodes | Optimal Path, Minimum Distance |
| 连通分量 | Connected Component | 图中相互可达的节点集合 | 连通子图、连通块 |
| Connected Component | Connected Component | Set of mutually reachable nodes in a graph | Connected Subgraph, Connected Block |

## 3. 语义分析 / Semantic Analysis

### 3.1 语义表示概念 / Semantic Representation Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 语义分析 | Semantic Analysis | 理解文本深层含义的分析过程 | 语义理解、意义分析 |
| Semantic Analysis | Semantic Analysis | Process of understanding deep meaning of text | Semantic Understanding, Meaning Analysis |
| 词嵌入 | Word Embedding | 将词汇映射到向量空间的技术 | 词向量、分布式表示 |
| Word Embedding | Word Embedding | Technique for mapping words to vector space | Word Vector, Distributed Representation |
| 语义相似性 | Semantic Similarity | 两个概念在语义上的相似程度 | 语义距离、相似度 |
| Semantic Similarity | Semantic Similarity | Degree of semantic similarity between two concepts | Semantic Distance, Similarity |
| 语义消歧 | Semantic Disambiguation | 确定多义词在特定语境中的含义 | 词义消歧、歧义消除 |
| Semantic Disambiguation | Semantic Disambiguation | Determining meaning of polysemous words in specific context | Word Sense Disambiguation, Ambiguity Resolution |
| 语义组合性 | Semantic Compositionality | 复杂表达式的语义由其组成部分组合而成 | 组合语义、语义组合 |
| Semantic Compositionality | Semantic Compositionality | Semantic of complex expressions composed from parts | Compositional Semantics, Semantic Composition |

### 3.2 语义模型概念 / Semantic Model Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 语义模型 | Semantic Model | 描述语义结构和关系的数学模型 | 语义框架、语义理论 |
| Semantic Model | Semantic Model | Mathematical model describing semantic structures and relations | Semantic Framework, Semantic Theory |
| 分布语义 | Distributional Semantics | 基于词汇共现的语义表示方法 | 统计语义、共现语义 |
| Distributional Semantics | Distributional Semantics | Semantic representation based on word co-occurrence | Statistical Semantics, Co-occurrence Semantics |
| 语义空间 | Semantic Space | 词汇和概念分布的向量空间 | 语义向量空间、概念空间 |
| Semantic Space | Semantic Space | Vector space where words and concepts are distributed | Semantic Vector Space, Concept Space |
| 语义映射 | Semantic Mapping | 不同语义空间之间的转换关系 | 语义转换、跨域映射 |
| Semantic Mapping | Semantic Mapping | Transformation between different semantic spaces | Semantic Transformation, Cross-domain Mapping |

## 4. 本体工程 / Ontology Engineering

### 4.1 本体构建概念 / Ontology Construction Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 本体工程 | Ontology Engineering | 构建和管理本体的系统化方法 | 本体构建、本体管理 |
| Ontology Engineering | Ontology Engineering | Systematic methodology for building and managing ontologies | Ontology Construction, Ontology Management |
| 本体构建 | Ontology Construction | 创建和定义本体概念体系的过程 | 本体设计、概念建模 |
| Ontology Construction | Ontology Construction | Process of creating and defining ontological conceptual systems | Ontology Design, Conceptual Modeling |
| 概念层次 | Concept Hierarchy | 概念间的分类和继承关系 | 概念树、分类体系 |
| Concept Hierarchy | Concept Hierarchy | Classification and inheritance relationships between concepts | Concept Tree, Classification System |
| 本体映射 | Ontology Mapping | 不同本体间概念对应关系的建立 | 本体对齐、概念映射 |
| Ontology Mapping | Ontology Mapping | Establishing concept correspondences between different ontologies | Ontology Alignment, Concept Mapping |
| 本体演化 | Ontology Evolution | 本体随时间和需求变化的更新过程 | 本体维护、本体更新 |
| Ontology Evolution | Ontology Evolution | Process of updating ontologies over time and requirements | Ontology Maintenance, Ontology Update |

### 4.2 本体质量概念 / Ontology Quality Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 本体质量 | Ontology Quality | 本体在准确性、完整性、一致性等方面的质量指标 | 质量评估、质量度量 |
| Ontology Quality | Ontology Quality | Quality metrics of ontology in accuracy, completeness, consistency | Quality Assessment, Quality Metrics |
| 本体评估 | Ontology Evaluation | 评估本体质量和适用性的过程 | 质量检查、适用性评估 |
| Ontology Evaluation | Ontology Evaluation | Process of evaluating ontology quality and applicability | Quality Check, Applicability Assessment |
| 本体一致性 | Ontology Consistency | 本体中概念和关系的一致程度 | 逻辑一致性、语义一致性 |
| Ontology Consistency | Ontology Consistency | Degree of consistency in concepts and relations within ontology | Logical Consistency, Semantic Consistency |
| 本体完整性 | Ontology Completeness | 本体覆盖领域知识的完整程度 | 覆盖度、完备性 |
| Ontology Completeness | Ontology Completeness | Degree of completeness in covering domain knowledge | Coverage, Completeness |

## 5. 知识抽取 / Knowledge Extraction

### 5.1 抽取方法概念 / Extraction Method Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 知识抽取 | Knowledge Extraction | 从非结构化数据中自动提取结构化知识的过程 | 信息抽取、知识发现 |
| Knowledge Extraction | Knowledge Extraction | Process of automatically extracting structured knowledge from unstructured data | Information Extraction, Knowledge Discovery |
| 实体识别 | Entity Recognition | 识别文本中特定类型实体的技术 | 命名实体识别、实体标注 |
| Entity Recognition | Entity Recognition | Technique for identifying specific types of entities in text | Named Entity Recognition, Entity Annotation |
| 关系抽取 | Relation Extraction | 识别实体间语义关系的技术 | 关系识别、关系标注 |
| Relation Extraction | Relation Extraction | Technique for identifying semantic relations between entities | Relation Recognition, Relation Annotation |
| 事件抽取 | Event Extraction | 识别和抽取文本中事件信息的技术 | 事件识别、事件标注 |
| Event Extraction | Event Extraction | Technique for identifying and extracting event information from text | Event Recognition, Event Annotation |
| 属性抽取 | Attribute Extraction | 抽取实体属性信息的技术 | 特征抽取、属性标注 |
| Attribute Extraction | Attribute Extraction | Technique for extracting entity attribute information | Feature Extraction, Attribute Annotation |

### 5.2 抽取技术概念 / Extraction Technology Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 监督学习 | Supervised Learning | 基于标注数据进行训练的学习方法 | 有监督学习、标注学习 |
| Supervised Learning | Supervised Learning | Learning method based on annotated training data | Supervised Learning, Annotation-based Learning |
| 无监督学习 | Unsupervised Learning | 不依赖标注数据的学习方法 | 无监督学习、聚类学习 |
| Unsupervised Learning | Unsupervised Learning | Learning method without relying on annotated data | Unsupervised Learning, Clustering Learning |
| 半监督学习 | Semi-supervised Learning | 结合少量标注数据和大量未标注数据的学习方法 | 半监督学习、弱监督学习 |
| Semi-supervised Learning | Semi-supervised Learning | Learning method combining small annotated data with large unannotated data | Semi-supervised Learning, Weakly Supervised Learning |
| 远程监督 | Distant Supervision | 利用知识库自动生成训练数据的监督方法 | 远程标注、自动标注 |
| Distant Supervision | Distant Supervision | Supervised method using knowledge base to automatically generate training data | Distant Annotation, Automatic Annotation |

## 6. 推理系统 / Reasoning Systems

### 6.1 推理方法概念 / Reasoning Method Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 推理系统 | Reasoning System | 基于已有知识进行逻辑推理的系统 | 推理引擎、推理机制 |
| Reasoning System | Reasoning System | System for logical reasoning based on existing knowledge | Reasoning Engine, Reasoning Mechanism |
| 符号推理 | Symbolic Reasoning | 基于符号和逻辑规则的推理方法 | 逻辑推理、规则推理 |
| Symbolic Reasoning | Symbolic Reasoning | Reasoning method based on symbols and logical rules | Logical Reasoning, Rule-based Reasoning |
| 统计推理 | Statistical Reasoning | 基于概率和统计模型的推理方法 | 概率推理、统计推断 |
| Statistical Reasoning | Statistical Reasoning | Reasoning method based on probability and statistical models | Probabilistic Reasoning, Statistical Inference |
| 混合推理 | Hybrid Reasoning | 结合符号和统计方法的推理技术 | 混合方法、集成推理 |
| Hybrid Reasoning | Hybrid Reasoning | Reasoning technique combining symbolic and statistical methods | Hybrid Method, Integrated Reasoning |
| 因果推理 | Causal Reasoning | 基于因果关系进行推理的方法 | 因果推断、因果分析 |
| Causal Reasoning | Causal Reasoning | Reasoning method based on causal relationships | Causal Inference, Causal Analysis |

### 6.2 推理技术概念 / Reasoning Technology Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 演绎推理 | Deductive Reasoning | 从一般到特殊的逻辑推理过程 | 演绎、逻辑推导 |
| Deductive Reasoning | Deductive Reasoning | Logical reasoning process from general to specific | Deduction, Logical Derivation |
| 归纳推理 | Inductive Reasoning | 从特殊到一般的推理过程 | 归纳、经验推理 |
| Inductive Reasoning | Inductive Reasoning | Reasoning process from specific to general | Induction, Empirical Reasoning |
| 类比推理 | Analogical Reasoning | 基于相似性进行推理的方法 | 类比、相似性推理 |
| Analogical Reasoning | Analogical Reasoning | Reasoning method based on similarity | Analogy, Similarity Reasoning |
| 默认推理 | Default Reasoning | 在信息不完全情况下的推理方法 | 默认逻辑、非单调推理 |
| Default Reasoning | Default Reasoning | Reasoning method under incomplete information | Default Logic, Non-monotonic Reasoning |

## 7. 应用系统 / Applications

### 7.1 应用领域概念 / Application Domain Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 智能问答 | Intelligent Question Answering | 基于知识图谱的自动问答系统 | 问答系统、智能客服 |
| Intelligent Question Answering | Intelligent Question Answering | Automatic question answering system based on knowledge graph | QA System, Intelligent Customer Service |
| 推荐系统 | Recommendation System | 基于用户偏好和知识图谱的推荐技术 | 个性化推荐、智能推荐 |
| Recommendation System | Recommendation System | Recommendation technology based on user preferences and knowledge graph | Personalized Recommendation, Intelligent Recommendation |
| 搜索引擎 | Search Engine | 基于知识图谱的语义搜索系统 | 语义搜索、智能搜索 |
| Search Engine | Search Engine | Semantic search system based on knowledge graph | Semantic Search, Intelligent Search |
| 决策支持 | Decision Support | 基于知识图谱的智能决策辅助系统 | 决策分析、智能决策 |
| Decision Support | Decision Support | Intelligent decision support system based on knowledge graph | Decision Analysis, Intelligent Decision |
| 知识管理 | Knowledge Management | 基于知识图谱的组织知识管理系统 | 知识组织、知识服务 |
| Knowledge Management | Knowledge Management | Organizational knowledge management system based on knowledge graph | Knowledge Organization, Knowledge Service |

### 7.2 系统架构概念 / System Architecture Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 系统架构 | System Architecture | 知识图谱应用系统的整体结构设计 | 架构设计、系统设计 |
| System Architecture | System Architecture | Overall structural design of knowledge graph application system | Architecture Design, System Design |
| 微服务架构 | Microservices Architecture | 基于微服务的分布式系统架构 | 服务化架构、分布式架构 |
| Microservices Architecture | Microservices Architecture | Distributed system architecture based on microservices | Service-oriented Architecture, Distributed Architecture |
| 数据存储 | Data Storage | 知识图谱数据的存储和管理方案 | 存储方案、数据管理 |
| Data Storage | Data Storage | Storage and management solution for knowledge graph data | Storage Solution, Data Management |
| 查询处理 | Query Processing | 知识图谱查询的解析和执行过程 | 查询优化、查询执行 |
| Query Processing | Query Processing | Process of parsing and executing knowledge graph queries | Query Optimization, Query Execution |
| 性能优化 | Performance Optimization | 提升系统性能的技术和方法 | 性能调优、系统优化 |
| Performance Optimization | Performance Optimization | Techniques and methods for improving system performance | Performance Tuning, System Optimization |

## 8. 形式化方法 / Formal Methods

### 8.1 形式化验证概念 / Formal Verification Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 形式化方法 | Formal Methods | 基于数学逻辑的系统验证和证明方法 | 形式化验证、数学证明 |
| Formal Methods | Formal Methods | Mathematical logic-based system verification and proof methods | Formal Verification, Mathematical Proof |
| 形式规约 | Formal Specification | 使用形式化语言描述系统需求和行为 | 形式化描述、规约语言 |
| Formal Specification | Formal Specification | Describing system requirements and behavior using formal languages | Formal Description, Specification Language |
| 定理证明 | Theorem Proving | 使用逻辑推理证明系统性质的数学方法 | 逻辑证明、数学证明 |
| Theorem Proving | Theorem Proving | Mathematical method for proving system properties using logical reasoning | Logical Proof, Mathematical Proof |
| 模型检查 | Model Checking | 自动验证系统模型是否满足规约的技术 | 模型验证、自动验证 |
| Model Checking | Model Checking | Technique for automatically verifying whether system models satisfy specifications | Model Verification, Automatic Verification |
| 抽象解释 | Abstract Interpretation | 基于抽象域的程序分析方法 | 抽象分析、程序分析 |
| Abstract Interpretation | Abstract Interpretation | Program analysis method based on abstract domains | Abstract Analysis, Program Analysis |

### 8.2 形式化语言概念 / Formal Language Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 形式化语言 | Formal Language | 具有严格语法和语义的数学语言 | 数学语言、逻辑语言 |
| Formal Language | Formal Language | Mathematical language with strict syntax and semantics | Mathematical Language, Logical Language |
| 一阶逻辑 | First-Order Logic | 包含量词和谓词的逻辑系统 | 谓词逻辑、量化逻辑 |
| First-Order Logic | First-Order Logic | Logical system including quantifiers and predicates | Predicate Logic, Quantified Logic |
| 描述逻辑 | Description Logic | 用于知识表示的逻辑系统 | 概念逻辑、知识逻辑 |
| Description Logic | Description Logic | Logical system for knowledge representation | Concept Logic, Knowledge Logic |
| 时态逻辑 | Temporal Logic | 描述时间相关性质的逻辑系统 | 时间逻辑、动态逻辑 |
| Temporal Logic | Temporal Logic | Logical system for describing time-related properties | Time Logic, Dynamic Logic |
| 模态逻辑 | Modal Logic | 包含模态算子的逻辑系统 | 可能世界逻辑、模态算子 |
| Modal Logic | Modal Logic | Logical system including modal operators | Possible Worlds Logic, Modal Operators |

## 9. 工程实践 / Engineering Practice

### 9.1 工程方法论概念 / Engineering Methodology Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 工程实践 | Engineering Practice | 知识图谱项目的工程化实施方法 | 工程方法、实践指南 |
| Engineering Practice | Engineering Practice | Engineering implementation methodology for knowledge graph projects | Engineering Method, Practice Guide |
| 敏捷开发 | Agile Development | 迭代式、增量式的软件开发方法 | 敏捷方法、迭代开发 |
| Agile Development | Agile Development | Iterative and incremental software development methodology | Agile Method, Iterative Development |
| 持续集成 | Continuous Integration | 频繁集成代码变更的开发实践 | 自动化集成、持续部署 |
| Continuous Integration | Continuous Integration | Development practice of frequently integrating code changes | Automated Integration, Continuous Deployment |
| 测试驱动开发 | Test-Driven Development | 先写测试再写代码的开发方法 | TDD、测试优先 |
| Test-Driven Development | Test-Driven Development | Development method of writing tests before code | TDD, Test-First |
| 代码审查 | Code Review | 对代码质量和正确性的审查过程 | 同行评审、代码检查 |
| Code Review | Code Review | Process of reviewing code quality and correctness | Peer Review, Code Inspection |

### 9.2 质量保证概念 / Quality Assurance Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 质量保证 | Quality Assurance | 确保项目质量达到标准的系统化方法 | 质量管理、质量控制 |
| Quality Assurance | Quality Assurance | Systematic method for ensuring project quality meets standards | Quality Management, Quality Control |
| 代码质量 | Code Quality | 代码在可读性、可维护性等方面的质量指标 | 代码标准、编程规范 |
| Code Quality | Code Quality | Quality metrics of code in readability, maintainability | Code Standards, Programming Standards |
| 性能测试 | Performance Testing | 评估系统性能的测试方法 | 压力测试、负载测试 |
| Performance Testing | Performance Testing | Testing method for evaluating system performance | Stress Testing, Load Testing |
| 安全测试 | Security Testing | 评估系统安全性的测试方法 | 漏洞测试、安全评估 |
| Security Testing | Security Testing | Testing method for evaluating system security | Vulnerability Testing, Security Assessment |
| 可用性测试 | Usability Testing | 评估系统易用性的测试方法 | 用户体验测试、易用性评估 |
| Usability Testing | Usability Testing | Testing method for evaluating system usability | User Experience Testing, Usability Assessment |

## 10. 研究方法论 / Research Methodology

### 10.1 研究方法概念 / Research Method Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 研究方法论 | Research Methodology | 指导科学研究的方法体系 | 研究方法、科学方法 |
| Research Methodology | Research Methodology | Methodological system guiding scientific research | Research Method, Scientific Method |
| 实验设计 | Experimental Design | 设计科学实验的方法和原则 | 实验方法、设计原则 |
| Experimental Design | Experimental Design | Methods and principles for designing scientific experiments | Experimental Method, Design Principles |
| 数据收集 | Data Collection | 收集研究所需数据的方法和过程 | 数据获取、信息收集 |
| Data Collection | Data Collection | Methods and processes for collecting research data | Data Acquisition, Information Collection |
| 数据分析 | Data Analysis | 对收集数据进行统计分析的方法 | 统计分析、数据处理 |
| Data Analysis | Data Analysis | Methods for statistical analysis of collected data | Statistical Analysis, Data Processing |
| 结果验证 | Result Validation | 验证研究结果正确性和可靠性的方法 | 结果检验、可靠性验证 |
| Result Validation | Result Validation | Methods for validating correctness and reliability of research results | Result Verification, Reliability Validation |

### 10.2 研究评估概念 / Research Evaluation Concepts

| 中文术语 | English Term | 定义 / Definition | 相关概念 / Related Concepts |
|---------|-------------|------------------|---------------------------|
| 研究评估 | Research Evaluation | 评估研究质量和价值的方法 | 质量评估、价值评估 |
| Research Evaluation | Research Evaluation | Methods for evaluating research quality and value | Quality Assessment, Value Assessment |
| 可重现性 | Reproducibility | 研究结果可以被其他研究者重现的程度 | 可重复性、可复制性 |
| Reproducibility | Reproducibility | Degree to which research results can be reproduced by other researchers | Repeatability, Replicability |
| 可验证性 | Verifiability | 研究过程和结果可以被验证的程度 | 可检验性、可证明性 |
| Verifiability | Verifiability | Degree to which research process and results can be verified | Testability, Provability |
| 系统性 | Systematicity | 研究方法的系统性和完整性 | 系统性方法、完整性 |
| Systematicity | Systematicity | Systematicity and completeness of research methods | Systematic Method, Completeness |
| 创新性 | Innovation | 研究在理论或方法上的创新程度 | 原创性、新颖性 |
| Innovation | Innovation | Degree of innovation in theory or methodology | Originality, Novelty |

---

**最后更新** / Last Updated: 2024年12月19日 / December 19, 2024
**版本** / Version: 1.0 / 1.0
**维护者** / Maintainer: 项目团队 / Project Team
