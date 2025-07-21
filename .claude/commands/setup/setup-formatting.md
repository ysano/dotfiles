# Setup Formatting Command

任意のプロジェクトでコードスタイルを統一するための自動コードフォーマットツールを設定します。

## 手順

以下のステップに従ってコードフォーマットを設定してください： **$ARGUMENTS**

1. **言語固有のツール**

   **JavaScript/TypeScript:**
   ```bash
   npm install -D prettier
   echo '{"semi": true, "singleQuote": true, "tabWidth": 2}' > .prettierrc
   ```

   **Python:**
   ```bash
   pip install black isort
   echo '[tool.black]\nline-length = 88\ntarget-version = ["py38"]' > pyproject.toml
   ```

   **Java:**
   ```bash
   # Google Java Format または Spotless plugin
   ```

2. **設定ファイル**

   **.prettierrc:**
   ```json
   {
     "semi": true,
     "singleQuote": true,
     "tabWidth": 2,
     "trailingComma": "es5",
     "printWidth": 80
   }
   ```

3. **IDE設定**
   - フォーマッター拡張機能をインストール
   - 保存時フォーマットを有効化
   - キーボードショートカットを設定

4. **スクリプトと自動化**
   ```json
   {
     "scripts": {
       "format": "prettier --write .",
       "format:check": "prettier --check ."
     }
   }
   ```

5. **Pre-commit Hooks**
   ```bash
   npm install -D husky lint-staged
   echo '{"*.{js,ts,tsx}": ["prettier --write", "eslint --fix"]}' > .lintstagedrc
   ```

最初にコードベース全体にフォーマットを実行し、チームのIDE設定を一貫して設定することを忘れないでください。