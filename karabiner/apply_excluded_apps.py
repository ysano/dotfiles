#!/usr/bin/env python3
"""Emacs Mode の除外アプリリストを karabiner.json に一括反映するスクリプト。

excluded_apps.json で定義した bundle_identifiers を、karabiner.json 内の
全 frontmost_application_unless 条件に適用する。

Usage:
    python3 apply_excluded_apps.py              # 適用
    python3 apply_excluded_apps.py --dry-run    # 変更箇所数の確認のみ
    python3 apply_excluded_apps.py --diff       # 変更差分を表示
"""

import argparse
import difflib
import json
import sys
from pathlib import Path


def load_excluded_apps(path: Path) -> list[str]:
    """excluded_apps.json から bundle_identifiers を読み込む。"""
    with open(path, encoding="utf-8") as f:
        data = json.load(f)
    ids = data["bundle_identifiers"]
    # 重複チェック
    if len(ids) != len(set(ids)):
        seen = set()
        dupes = [x for x in ids if x in seen or seen.add(x)]
        print(f"警告: excluded_apps.json に重複があります: {dupes}", file=sys.stderr)
    return ids


def patch_karabiner(karabiner_data: dict, new_ids: list[str]) -> int:
    """karabiner.json データ内の全 frontmost_application_unless を置換。

    Returns:
        置換した条件の数。
    """
    count = 0
    for profile in karabiner_data.get("profiles", []):
        for rule in profile.get("complex_modifications", {}).get("rules", []):
            if "[Emacs Mode" not in rule.get("description", ""):
                continue
            for manipulator in rule.get("manipulators", []):
                for condition in manipulator.get("conditions", []):
                    if condition.get("type") == "frontmost_application_unless":
                        condition["bundle_identifiers"] = list(new_ids)
                        count += 1
    return count


def main():
    parser = argparse.ArgumentParser(
        description="Emacs Mode の除外アプリリストを karabiner.json に一括反映"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="変更箇所数を確認するだけで、ファイルを書き換えない",
    )
    parser.add_argument(
        "--diff",
        action="store_true",
        help="変更差分を表示する（ファイルは書き換えない）",
    )
    args = parser.parse_args()

    script_dir = Path(__file__).resolve().parent
    excluded_path = script_dir / "excluded_apps.json"
    karabiner_path = script_dir / "karabiner.json"

    if not excluded_path.exists():
        print(f"エラー: {excluded_path} が見つかりません", file=sys.stderr)
        sys.exit(1)
    if not karabiner_path.exists():
        print(f"エラー: {karabiner_path} が見つかりません", file=sys.stderr)
        sys.exit(1)

    new_ids = load_excluded_apps(excluded_path)
    print(f"除外アプリ数: {len(new_ids)}")

    with open(karabiner_path, encoding="utf-8") as f:
        original_text = f.read()
    karabiner_data = json.loads(original_text)

    count = patch_karabiner(karabiner_data, new_ids)
    print(f"置換対象: {count} 箇所")

    new_text = json.dumps(karabiner_data, ensure_ascii=False, indent=4) + "\n"

    if original_text == new_text:
        print("変更なし")
        return

    if args.diff or args.dry_run:
        if args.diff:
            diff = difflib.unified_diff(
                original_text.splitlines(keepends=True),
                new_text.splitlines(keepends=True),
                fromfile="karabiner.json (before)",
                tofile="karabiner.json (after)",
            )
            sys.stdout.writelines(diff)
        if args.dry_run:
            print("(dry-run: ファイルは変更されていません)")
        return

    with open(karabiner_path, "w", encoding="utf-8") as f:
        f.write(new_text)
    print("karabiner.json を更新しました")


if __name__ == "__main__":
    main()
