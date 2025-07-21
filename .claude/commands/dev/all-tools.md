# 利用可能な開発ツール一覧表示

利用可能な開発ツール一覧を表示します

*コマンド原作者: IndyDevDan (YouTube: https://www.youtube.com/@indydevdan) / DislerH (GitHub: https://github.com/disler)*

## 実行手順

システムプロンプトから利用可能なすべてのツールを以下の形式で表示してください：

1. **各ツールをリスト表示** - TypeScript関数シグネチャと共に
2. **目的を含める** - 各ツールの目的をサフィックスとして
3. **二重改行を使用** - 読みやすさのため
4. **箇条書き形式** - 明確な整理のため

出力は開発者が以下を理解できるようにします：
- 現在のClaude Codeセッションで利用可能なツール
- 参照用の正確な関数シグネチャ
- 各ツールの主要な目的

出力形式例：
```typescript
• functionName(parameters: Type): ReturnType - ツールの目的

• anotherFunction(params: ParamType): ResultType - このツールが行うこと
```

このコマンドは以下の用途で有用です：
- 利用可能な機能のクイックリファレンス
- ツールシグネチャの理解
- 特定のタスクに使用するツールの計画