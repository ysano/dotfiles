"""Shared fixtures for observability script tests."""
import importlib.util
import os
import sys

import pytest

# --- Add test directory to sys.path for helpers import ---
_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
if _TEST_DIR not in sys.path:
    sys.path.insert(0, _TEST_DIR)

# --- Import hyphenated scripts via importlib ---
_SCRIPT_DIR = os.path.dirname(_TEST_DIR)


def _import_script(filename, module_name):
    """Import a hyphenated script as a Python module."""
    path = os.path.join(_SCRIPT_DIR, filename)
    spec = importlib.util.spec_from_file_location(module_name, path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = mod
    spec.loader.exec_module(mod)
    return mod


aggregate_sprint = _import_script("aggregate-sprint.py", "aggregate_sprint")
analyze_trend = _import_script("analyze-trend.py", "analyze_trend")


# --- Threshold fixtures ---

@pytest.fixture
def solo_thresholds():
    return aggregate_sprint.get_dora_thresholds("solo")


@pytest.fixture
def pod_thresholds():
    return aggregate_sprint.get_dora_thresholds("pod")


@pytest.fixture
def squad_thresholds():
    return aggregate_sprint.get_dora_thresholds("squad")
