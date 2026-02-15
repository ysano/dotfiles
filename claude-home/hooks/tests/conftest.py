"""Test configuration for hooks tests."""
import importlib
import os
import sys

# Add hooks directory to sys.path so hyphenated filenames can be imported
hooks_dir = os.path.join(os.path.dirname(__file__), "..")
sys.path.insert(0, hooks_dir)

# Import hyphenated modules using importlib and register as clean names
import importlib.util


def _import_hyphenated(file_name, module_name):
    """Import a Python file with hyphens in the name under a clean module name."""
    spec = importlib.util.spec_from_file_location(
        module_name, os.path.join(hooks_dir, file_name)
    )
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


_import_hyphenated("churn-counter.py", "churn_counter")
_import_hyphenated("check-spec-existence.py", "check_spec_existence")
_import_hyphenated("metrics-collector.py", "metrics_collector")
