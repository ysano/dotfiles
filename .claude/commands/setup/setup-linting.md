# Setup Linting Command

一貫したコード標準を維持するために、任意のプロジェクトに包括的なlintingとコード品質ツールを設定します。

## 手順

linting設定に以下の体系的なアプローチに従ってください： **$ARGUMENTS**

1. **プロジェクト分析**
   - プログラミング言語とフレームワークを特定
   - 既存のlinting設定を確認
   - 現在のコードスタイルとパターンを確認
   - チームの好みと要件を評価

2. **言語別ツール選択**

   **JavaScript/TypeScript:**
   ```bash
   npm install -D eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
   npm install -D prettier eslint-config-prettier eslint-plugin-prettier
   ```

   **Python:**
   ```bash
   pip install flake8 black isort mypy pylint
   ```

   **Java:**
   ```bash
   # pom.xml または build.gradle に追加
   # Checkstyle, SpotBugs, PMD
   ```

3. **設定セットアップ**

   **ESLint (.eslintrc.json):**
   ```json
   {
     "extends": [
       "eslint:recommended",
       "@typescript-eslint/recommended",
       "prettier"
     ],
     "parser": "@typescript-eslint/parser",
     "plugins": ["@typescript-eslint"],
     "rules": {
       "no-console": "warn",
       "no-unused-vars": "error",
       "@typescript-eslint/no-explicit-any": "warn"
     }
   }
   ```

4. **IDE統合**
   - VS Code設定を構成
   - 保存時自動修正を設定
   - 関連する拡張機能をインストール

5. **CI/CD統合**
   ```yaml
   - name: Lint code
     run: npm run lint
   ```

6. **Package.json Scripts**
   ```json
   {
     "scripts": {
       "lint": "eslint src --ext .ts,.tsx",
       "lint:fix": "eslint src --ext .ts,.tsx --fix",
       "format": "prettier --write src"
     }
   }
   ```

チームの好みに基づいてルールをカスタマイズし、段階的により厳格な標準を実施することを忘れないでください。