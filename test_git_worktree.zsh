#!/usr/bin/env zsh
# Git Worktreeコマンド群のテストスクリプト
# Claude Code対応版 - 2025年ベストプラクティス準拠

# テスト設定
TEST_DIR="/tmp/gwt_test_$(date +%s)"
TEST_REPO="test_repo"
REPO_PATH="$TEST_DIR/$TEST_REPO"

# 色付き出力用
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# テスト結果カウンタ
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0

# ヘルパー関数
print_test_header() {
    echo -e "${BLUE}🧪 $1${NC}"
    echo "========================================"
}

print_test_result() {
    local test_name="$1"
    local result="$2"
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    
    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✅ $test_name: PASS${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}❌ $test_name: FAIL${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

print_section() {
    echo ""
    echo -e "${YELLOW}📋 $1${NC}"
    echo "----------------------------------------"
}

# テスト環境セットアップ
setup_test_environment() {
    print_section "テスト環境セットアップ"
    
    # テストディレクトリ作成
    mkdir -p "$TEST_DIR"
    cd "$TEST_DIR"
    
    # テスト用Gitリポジトリ作成
    git init "$TEST_REPO"
    cd "$REPO_PATH"
    
    # 初期コミット作成
    echo "# Test Repository" > README.md
    git add README.md
    git commit -m "Initial commit"
    
    # 追加ブランチ作成
    git checkout -b feature/test-branch
    echo "Feature content" > feature.txt
    git add feature.txt
    git commit -m "Add feature"
    git checkout main 2>/dev/null || git checkout master
    
    echo "✅ テスト環境セットアップ完了"
    echo "   📂 テストディレクトリ: $TEST_DIR"
    echo "   📂 テストリポジトリ: $REPO_PATH"
}

# テスト環境クリーンアップ
cleanup_test_environment() {
    print_section "テスト環境クリーンアップ"
    
    cd /
    rm -rf "$TEST_DIR"
    
    echo "✅ テスト環境クリーンアップ完了"
}

# git-worktree.zshを読み込み
load_gwt_functions() {
    print_section "Git Worktree機能読み込み"
    
    local gwt_script="$HOME/dotfiles/.zsh/git-worktree.zsh"
    if [[ -f "$gwt_script" ]]; then
        source "$gwt_script"
        echo "✅ Git Worktree機能読み込み完了"
    else
        echo "❌ git-worktree.zsh が見つかりません: $gwt_script"
        exit 1
    fi
}

# テスト1: ヘルプ機能
test_help_function() {
    print_test_header "ヘルプ機能テスト"
    
    local help_output=$(gwt help 2>&1)
    if [[ "$help_output" =~ "Git Worktree管理コマンド" ]]; then
        print_test_result "ヘルプ表示" "PASS"
    else
        print_test_result "ヘルプ表示" "FAIL"
        echo "出力: $help_output"
    fi
}

# テスト2: 無効なコマンドエラーハンドリング
test_invalid_command() {
    print_test_header "無効なコマンドエラーハンドリング"
    
    local error_output=$(gwt invalid_command 2>&1)
    if [[ "$error_output" =~ "不明なコマンド" ]]; then
        print_test_result "無効なコマンドエラー" "PASS"
    else
        print_test_result "無効なコマンドエラー" "FAIL"
        echo "出力: $error_output"
    fi
}

# テスト3: worktree作成
test_worktree_create() {
    print_test_header "Worktree作成テスト"
    
    cd "$REPO_PATH"
    
    # 新しいブランチでworktree作成
    local create_output=$(gwt create test-feature 2>&1)
    if [[ $? -eq 0 ]] && [[ "$create_output" =~ "Worktree作成完了" ]]; then
        print_test_result "新規ブランチworktree作成" "PASS"
    else
        print_test_result "新規ブランチworktree作成" "FAIL"
        echo "出力: $create_output"
    fi
    
    # 既存ブランチでworktree作成
    local existing_create_output=$(gwt create feature/test-branch 2>&1)
    if [[ $? -eq 0 ]] && [[ "$existing_create_output" =~ "既存ブランチ" ]]; then
        print_test_result "既存ブランチworktree作成" "PASS"
    else
        print_test_result "既存ブランチworktree作成" "FAIL"
        echo "出力: $existing_create_output"
    fi
}

# テスト4: worktree一覧表示
test_worktree_list() {
    print_test_header "Worktree一覧表示テスト"
    
    cd "$REPO_PATH"
    
    local list_output=$(gwt list 2>&1)
    if [[ "$list_output" =~ "Git Worktree一覧" ]] && [[ "$list_output" =~ "test-feature" ]]; then
        print_test_result "worktree一覧表示" "PASS"
    else
        print_test_result "worktree一覧表示" "FAIL"
        echo "出力: $list_output"
    fi
}

# テスト5: worktree削除
test_worktree_remove() {
    print_test_header "Worktree削除テスト"
    
    cd "$REPO_PATH"
    
    # 自動的に"y"を入力してworktreeを削除
    echo "y" | gwt remove test-feature >/dev/null 2>&1
    local remove_result=$?
    
    # 削除後のworktree一覧チェック
    local list_after_remove=$(git worktree list 2>&1)
    if [[ $remove_result -eq 0 ]] && [[ ! "$list_after_remove" =~ "test-feature" ]]; then
        print_test_result "worktree削除" "PASS"
    else
        print_test_result "worktree削除" "FAIL"
        echo "削除結果: $remove_result"
        echo "削除後一覧: $list_after_remove"
    fi
}

# テスト6: メンテナンス機能
test_worktree_clean() {
    print_test_header "メンテナンス機能テスト"
    
    cd "$REPO_PATH"
    
    local clean_output=$(gwt clean 2>&1)
    if [[ "$clean_output" =~ "メンテナンス完了" ]]; then
        print_test_result "メンテナンス機能" "PASS"
    else
        print_test_result "メンテナンス機能" "FAIL"
        echo "出力: $clean_output"
    fi
}

# テスト7: Gitリポジトリ外でのエラーハンドリング
test_non_git_directory() {
    print_test_header "非Gitディレクトリエラーハンドリング"
    
    cd "$TEST_DIR"
    
    local error_output=$(gwt list 2>&1)
    if [[ "$error_output" =~ "Gitリポジトリ内で実行してください" ]]; then
        print_test_result "非Gitディレクトリエラー" "PASS"
    else
        print_test_result "非Gitディレクトリエラー" "FAIL"
        echo "出力: $error_output"
    fi
}

# テスト8: エイリアス機能
test_aliases() {
    print_test_header "エイリアス機能テスト"
    
    cd "$REPO_PATH"
    
    # gwcエイリアスのテスト（createの短縮）
    local alias_output=$(gwt c alias-test 2>&1)
    if [[ $? -eq 0 ]] && [[ "$alias_output" =~ "Worktree作成完了" ]]; then
        print_test_result "createエイリアス(c)" "PASS"
    else
        print_test_result "createエイリアス(c)" "FAIL"
        echo "出力: $alias_output"
    fi
    
    # gwlエイリアスのテスト（listの短縮）
    local list_alias_output=$(gwt l 2>&1)
    if [[ "$list_alias_output" =~ "Git Worktree一覧" ]]; then
        print_test_result "listエイリアス(l)" "PASS"
    else
        print_test_result "listエイリアス(l)" "FAIL"
        echo "出力: $list_alias_output"
    fi
}

# テスト9: Gitオプション伝達テスト
test_git_options() {
    print_test_header "Gitオプション伝達テスト"
    
    cd "$REPO_PATH"
    
    # worktree作成してファイルを変更（未コミット状態）
    gwt create dirty-test >/dev/null 2>&1
    local worktree_path="../worktrees/test_repo-dirty-test"
    
    if [[ -d "$worktree_path" ]]; then
        # 未コミットファイルを作成
        echo "dirty content" > "$worktree_path/dirty.txt"
        
        # 通常の削除（失敗するはず）
        local remove_output=$(echo "y" | gwt remove dirty-test 2>&1)
        
        # --forceオプションでの削除
        echo "y" | gwt remove --force dirty-test >/dev/null 2>&1
        local force_result=$?
        
        if [[ $force_result -eq 0 ]]; then
            print_test_result "--forceオプション" "PASS"
        else
            print_test_result "--forceオプション" "FAIL"
            echo "force削除結果: $force_result"
        fi
    else
        print_test_result "--forceオプション" "FAIL"
        echo "テスト用worktreeが作成されませんでした"
    fi
}

# テスト結果サマリー表示
print_test_summary() {
    echo ""
    echo "========================================"
    echo -e "${BLUE}📊 テスト結果サマリー${NC}"
    echo "========================================"
    echo -e "総テスト数: ${YELLOW}$TESTS_TOTAL${NC}"
    echo -e "成功: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "失敗: ${RED}$TESTS_FAILED${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}🎉 すべてのテストが成功しました！${NC}"
        echo ""
        echo -e "${BLUE}💡 Claude Code使用例:${NC}"
        echo "  gwt create feature/my-feature"
        echo "  cd ../worktrees/dotfiles-feature-my-feature"
        echo "  claude-code"
        return 0
    else
        echo -e "${RED}⚠️  一部のテストが失敗しました${NC}"
        return 1
    fi
}

# テスト12: リモートブランチ一覧表示
test_remote_branch_list() {
    print_test_header "リモートブランチ一覧表示テスト"
    
    cd "$REPO_PATH"
    
    # 基本的なリモートブランチ一覧
    local remote_output=$(gwt list --remote 2>&1)
    if [[ "$remote_output" =~ "リモートブランチ一覧" ]] && ([[ "$remote_output" =~ "origin" ]] || [[ "$remote_output" =~ "リモートブランチが見つかりませんでした" ]]); then
        print_test_result "リモートブランチ一覧表示" "PASS"
    else
        print_test_result "リモートブランチ一覧表示" "FAIL"
        echo "出力: $remote_output"
    fi
    
    # 短縮形-rオプション
    local remote_short_output=$(gwt list -r 2>&1)
    if [[ "$remote_short_output" =~ "リモートブランチ一覧" ]]; then
        print_test_result "リモートブランチ一覧(-r)" "PASS"
    else
        print_test_result "リモートブランチ一覧(-r)" "FAIL"
        echo "出力: $remote_short_output"
    fi
    
    # 詳細表示
    local remote_verbose_output=$(gwt list --remote --verbose 2>&1)
    if [[ "$remote_verbose_output" =~ "リモートブランチ詳細一覧" ]] && ([[ "$remote_verbose_output" =~ "最新:" ]] || [[ "$remote_verbose_output" =~ "リモートブランチが見つかりませんでした" ]]); then
        print_test_result "リモートブランチ詳細表示" "PASS"
    else
        print_test_result "リモートブランチ詳細表示" "FAIL"
        echo "出力: $remote_verbose_output"
    fi
}

# メイン実行
main() {
    echo -e "${BLUE}🚀 Git Worktree管理コマンド テストスイート${NC}"
    echo "Claude Code対応版 - 2025年ベストプラクティス準拠"
    echo ""
    
    # 前提条件チェック
    if ! command -v git >/dev/null 2>&1; then
        echo -e "${RED}❌ Gitがインストールされていません${NC}"
        exit 1
    fi
    
    # テスト実行
    load_gwt_functions
    setup_test_environment
    
    test_help_function
    test_invalid_command
    test_worktree_create
    test_worktree_list
    test_worktree_remove
    test_worktree_clean
    test_non_git_directory
    test_aliases
    test_git_options
    test_remote_branch_list
    
    cleanup_test_environment
    print_test_summary
}

# スクリプトが直接実行された場合のみmainを実行
if [[ "${(%):-%x}" == "${0}" ]]; then
    main "$@"
fi