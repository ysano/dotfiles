"""Shared fixtures for aggregate-sprint tests."""
import importlib.util
import os
import sys

import pytest

# --- Add test directory to sys.path for helpers import ---
_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
if _TEST_DIR not in sys.path:
    sys.path.insert(0, _TEST_DIR)

# --- Import aggregate-sprint.py (hyphenated filename) via importlib ---
_SCRIPT_DIR = os.path.dirname(_TEST_DIR)
_SCRIPT_PATH = os.path.join(_SCRIPT_DIR, "aggregate-sprint.py")

spec = importlib.util.spec_from_file_location("aggregate_sprint", _SCRIPT_PATH)
aggregate_sprint = importlib.util.module_from_spec(spec)
sys.modules["aggregate_sprint"] = aggregate_sprint
spec.loader.exec_module(aggregate_sprint)


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
