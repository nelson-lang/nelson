import os
import sys
import unittest
from io import StringIO
from pathlib import Path


repo_root = Path(__file__).resolve().parents[3]
package_root = Path(__file__).resolve().parents[1] / "resources" / "python"
for path in (repo_root, package_root):
    if str(path) not in sys.path:
        sys.path.insert(0, str(path))

os.environ.setdefault("NELSON_ENGINE_RUN_LIVE_TESTS", "0")

suite = unittest.defaultTestLoader.loadTestsFromName(
    "modules.python_engine.tests.test_nelson_engine_api"
)
stream = StringIO()
result = unittest.TextTestRunner(stream=stream, verbosity=2).run(suite)
status = 0 if result.wasSuccessful() else 1
report = stream.getvalue()
summary = {"status": status, "report": report}
if status != 0:
    print(report)
