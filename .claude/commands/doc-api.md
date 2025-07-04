# APIドキュメント生成コマンド

任意のプログラミング言語とフレームワーク用のコードから包括的なAPIドキュメントを生成します。

## 手順

APIドキュメント作成の体系的なアプローチに従ってください： **$ARGUMENTS**

1. **コード分析と発見**
   - コードベースをスキャンしてAPIエンドポイント、ルート、ハンドラーを検出
   - REST API、GraphQLスキーマ、RPCサービスを特定
   - コントローラークラス、ルート定義、ミドルウェアをマッピング
   - リクエスト/レスポンスモデルとデータ構造を発見

2. **ドキュメントツール選択**
   - スタックに基づいて適切なドキュメントツールを選択：
     - **OpenAPI/Swagger**: インタラクティブドキュメント付きREST API
     - **GraphQL**: GraphiQL、GraphQL Playground、Apollo Studio
     - **Postman**: APIコレクションとドキュメント
     - **Insomnia**: API設計とドキュメント
     - **Redoc**: 代替OpenAPIレンダラー
     - **API Blueprint**: MarkdownベースAPIドキュメント

3. **API仕様生成**
   
   **OpenAPIを使用したREST API用:**
   ```yaml
   openapi: 3.0.0
   info:
     title: $ARGUMENTS API
     version: 1.0.0
     description: $ARGUMENTS用包括的API
   servers:
     - url: https://api.example.com/v1
   paths:
     /users:
       get:
         summary: ユーザー一覧取得
         parameters:
           - name: page
             in: query
             schema:
               type: integer
         responses:
           '200':
             description: 成功レスポンス
             content:
               application/json:
                 schema:
                   type: array
                   items:
                     $ref: '#/components/schemas/User'
   components:
     schemas:
       User:
         type: object
         properties:
           id:
             type: integer
           name:
             type: string
           email:
             type: string
   ```

4. **エンドポイントドキュメント**
   - すべてのHTTPメソッド（GET、POST、PUT、DELETE、PATCH）を文書化
   - リクエストパラメータ（パス、クエリ、ヘッダー、ボディ）を指定
   - レスポンススキーマとステータスコードを定義
   - エラーレスポンスとエラーコードを含める
   - 認証と認可要件を文書化

5. **リクエスト/レスポンス例**
   - 各エンドポイントの現実的なリクエスト例を提供
   - 適切な形式のサンプルレスポンスデータを含める
   - 異なるレスポンスシナリオ（成功、エラー、エッジケース）を表示
   - コンテンツタイプとエンコーディングを文書化

6. **認証ドキュメント**
   - 認証方法（APIキー、JWT、OAuth）を文書化
   - 認可スコープと権限を説明
   - 認証例とトークン形式を提供
   - セッション管理とリフレッシュトークンフローを文書化

7. **データモデルドキュメント**
   - すべてのデータスキーマとモデルを定義
   - フィールドタイプ、制約、検証ルールを文書化
   - エンティティ間の関係を含める
   - データ構造の例を提供

8. **エラーハンドリングドキュメント**
   - すべての可能なエラーレスポンスを文書化
   - エラーコードとその意味を説明
   - トラブルシューティングガイダンスを提供
   - レート制限とスロットリング情報を含める

9. **インタラクティブドキュメント設定**
   
   **Swagger UI統合:**
   ```html
   <!DOCTYPE html>
   <html>
   <head>
     <title>APIドキュメント</title>
     <link rel="stylesheet" type="text/css" href="./swagger-ui-bundle.css" />
   </head>
   <body>
     <div id="swagger-ui"></div>
     <script src="./swagger-ui-bundle.js"></script>
     <script>
       SwaggerUIBundle({
         url: './api-spec.yaml',
         dom_id: '#swagger-ui'
       });
     </script>
   </body>
   </html>
   ```

10. **コード注釈とコメント**
    - APIハンドラーにインラインドキュメントを追加
    - フレームワーク固有の注釈ツールを使用:
      - **Java**: @ApiOperation, @ApiParam (Swagger注釈)
      - **Python**: FastAPIまたはFlask-RESTXでのDocstrings
      - **Node.js**: swagger-jsdocでのJSDocコメント
      - **C#**: XMLドキュメントコメント

11. **自動ドキュメント生成**
    
    **Node.js/Express用:**
    ```javascript
    const swaggerJsdoc = require('swagger-jsdoc');
    const swaggerUi = require('swagger-ui-express');
    
    const options = {
      definition: {
        openapi: '3.0.0',
        info: {
          title: 'APIドキュメント',
          version: '1.0.0',
        },
      },
      apis: ['./routes/*.js'],
    };
    
    const specs = swaggerJsdoc(options);
    app.use('/api-docs', swaggerUi.serve, swaggerUi.setup(specs));
    ```

12. **テスト統合**
    - ドキュメントからAPIテストコレクションを生成
    - テストスクリプトと検証ルールを含める
    - 自動APIテストを設定
    - テストシナリオと期待される結果を文書化

13. **バージョン管理**
    - APIバージョニング戦略を文書化
    - 複数のAPIバージョンのドキュメントを維持
    - 非推奨タイムラインと移行ガイドを文書化
    - バージョン間の破壊的変更を追跡

14. **パフォーマンスドキュメント**
    - レート制限とスロットリングポリシーを文書化
    - パフォーマンスベンチマークとSLAを含める
    - キャッシュ戦略とヘッダーを文書化
    - ページネーションとフィルタリングオプションを説明

15. **SDKとクライアントライブラリドキュメント**
    - API仕様からクライアントライブラリを生成
    - SDK使用法と例を文書化
    - 異なる言語のクイックスタートガイドを提供
    - 統合例とベストプラクティスを含める

16. **環境固有ドキュメント**
    - 異なる環境（開発、ステージング、本番）を文書化
    - 環境固有のエンドポイントと設定を含める
    - デプロイメントと設定要件を文書化
    - 環境設定手順を提供

17. **セキュリティドキュメント**
    - セキュリティベストプラクティスを文書化
    - CORSとCSPポリシーを含める
    - 入力検証とサニタイゼーションを文書化
    - セキュリティヘッダーとその目的を説明

18. **メンテナンスと更新**
    - 自動ドキュメント更新を設定
    - ドキュメントを最新に保つプロセスを作成
    - ドキュメントを定期的にレビューし検証
    - 開発ワークフローにドキュメントレビューを統合

**フレームワーク固有の例:**

**FastAPI (Python):**
```python
from fastapi import FastAPI
from pydantic import BaseModel

app = FastAPI(title="My API", version="1.0.0")

class User(BaseModel):
    id: int
    name: str
    email: str

@app.get("/users/{user_id}", response_model=User)
async def get_user(user_id: int):
    """IDでユーザーを取得。"""
    return {"id": user_id, "name": "John", "email": "john@example.com"}
```

**Spring Boot (Java):**
```java
@RestController
@Api(tags = "Users")
public class UserController {
    
    @GetMapping("/users/{id}")
    @ApiOperation(value = "IDでユーザーを取得")
    public ResponseEntity<User> getUser(
        @PathVariable @ApiParam("ユーザーID") Long id) {
        // 実装
    }
}
```

ドキュメントをコード変更に合わせて最新に保ち、内部チームと外部利用者の両方が簡単にアクセスできるようにすることを忘れないでください。