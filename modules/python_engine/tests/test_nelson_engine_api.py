import importlib
import io
import os
import subprocess
import sys
import tempfile
import time
import unittest
import xml.etree.ElementTree as ET
from concurrent.futures import Future, ThreadPoolExecutor
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1] / "resources" / "python"
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))


def _import_or_install(module_name, package_name=None):
    try:
        return importlib.import_module(module_name)
    except Exception:
        package_name = package_name or module_name
        try:
            subprocess.check_call([sys.executable, "-m", "pip", "install", package_name])
            return importlib.import_module(module_name)
        except Exception as exc:
            raise unittest.SkipTest(f"{package_name} is not available and could not be installed") from exc


class NelsonEngineApiTests(unittest.TestCase):
    def test_python_engine_xml_help_files_are_well_formed(self):
        root = Path(__file__).resolve().parents[1] / "help"
        for language in ("en_US", "fr_FR"):
            ET.parse(root / language / "xml" / "5_call_nelson_from_python.xml")
            ET.parse(root / language / "xml" / "6_install_nelson_engine_for_python.xml")

    def test_windows_installer_includes_python_engine_package_files(self):
        module_root = Path(__file__).resolve().parents[1]
        installer = (module_root / "module.iss").read_text(encoding="utf-8")
        self.assertIn(r"resources\python\*.toml", installer)
        self.assertIn(r"resources\python\*.md", installer)
        self.assertIn(r"resources\python\nelson\*.py", installer)
        self.assertIn(r"resources\python\nelson\engine\*.py", installer)

    def test_installed_package_smoke(self):
        if os.environ.get("NELSON_ENGINE_RUN_INSTALL_TESTS") != "1":
            raise unittest.SkipTest("set NELSON_ENGINE_RUN_INSTALL_TESTS=1 to run install smoke test")
        module_root = Path(__file__).resolve().parents[1]
        package_root = module_root / "resources" / "python"
        nelson_root = module_root.parents[1]
        dll = nelson_root / "bin" / "x64" / "libnlsEngine.dll"
        if not dll.exists():
            raise unittest.SkipTest("Nelson engine DLL is not available")
        if importlib.util.find_spec("setuptools") is None:
            raise unittest.SkipTest("setuptools is required to build the local nelson Python package")

        with tempfile.TemporaryDirectory() as folder:
            target = Path(folder) / "site"
            cwd = Path(folder) / "outside"
            cwd.mkdir()
            subprocess.check_call([
                sys.executable,
                "-m",
                "pip",
                "install",
                "--no-build-isolation",
                "--target",
                str(target),
                str(package_root),
            ])
            script = (
                "import nelson, nelson.engine; "
                "eng = nelson.engine.start_nelson(); "
                "value = eng.sqrt(4.0); "
                "eng.quit(); "
                "assert list(value) == [[2.0]], value"
            )
            env = os.environ.copy()
            env["PYTHONPATH"] = str(target)
            env["NELSON_ROOT"] = str(nelson_root)
            env["PATH"] = str(dll.parent) + os.pathsep + env.get("PATH", "")
            subprocess.check_call([sys.executable, "-c", script], cwd=str(cwd), env=env)

    def test_public_api_uses_nelson_names_only(self):
        engine = importlib.import_module("nelson.engine")

        self.assertTrue(hasattr(engine, "start_nelson"))
        self.assertTrue(hasattr(engine, "find_nelson"))
        self.assertTrue(hasattr(engine, "connect_nelson"))
        self.assertTrue(hasattr(engine, "NelsonEngine"))
        self.assertTrue(hasattr(engine, "FutureResult"))
        self.assertTrue(hasattr(engine, "TimeoutError"))
        self.assertTrue(hasattr(engine, "CancelledError"))
        self.assertTrue(hasattr(engine, "InterruptedError"))
        self.assertTrue(hasattr(engine, "RejectedExecutionError"))

        public_names = set(engine.__all__)
        self.assertIn("start_nelson", public_names)
        self.assertIn("NelsonEngine", public_names)
        self.assertNotIn("_to_engine_json", public_names)
        self.assertNotIn("_from_engine_json", public_names)
        self.assertNotIn("start_matlab", public_names)
        self.assertNotIn("find_matlab", public_names)
        self.assertNotIn("connect_matlab", public_names)
        self.assertNotIn("MatlabEngine", public_names)

    def test_top_level_public_api_exports_no_matlab_names(self):
        import nelson

        public_names = set(nelson.__all__)
        self.assertIn("double", public_names)
        self.assertIn("table", public_names)
        self.assertIn("struct", public_names)
        self.assertNotIn("matlab", public_names)
        self.assertFalse(any("matlab" in name.lower() for name in public_names))

    def test_library_discovery_uses_explicit_engine_library(self):
        import nelson.engine as engine

        with tempfile.TemporaryDirectory() as folder:
            dll = Path(folder) / engine._library_names()[0]
            dll.write_bytes(b"")
            with mock.patch.dict(os.environ, {"NELSON_ENGINE_LIBRARY": str(dll)}, clear=False):
                self.assertEqual(engine._find_library_path(), dll)

    def test_library_discovery_uses_nelson_root(self):
        import nelson.engine as engine

        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            dll = root / "bin" / "x64" / engine._library_names()[0]
            dll.parent.mkdir(parents=True)
            dll.write_bytes(b"")
            with mock.patch.dict(os.environ, {"NELSON_ROOT": str(root)}, clear=False):
                with mock.patch.object(engine, "_candidate_roots", return_value=[root]):
                    self.assertEqual(engine._find_library_path(), dll)

    def test_library_discovery_error_is_actionable(self):
        import nelson.engine as engine

        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            with mock.patch.dict(os.environ, {"NELSON_ROOT": str(root)}, clear=False):
                with mock.patch.object(engine, "_candidate_roots", return_value=[root]):
                    with self.assertRaisesRegex(engine.EngineError, "NELSON_ENGINE_LIBRARY"):
                        engine._find_library_path()

    def test_array_constructor_size_indexing_and_column_major_reshape(self):
        import nelson

        value = nelson.int16([[1, 2, 3], [4, 5, 6]])

        self.assertEqual(value.size, (2, 3))
        self.assertEqual(value.itemsize, 2)
        self.assertEqual(value[0][2], 3)
        self.assertEqual(value[1][2], 6)
        self.assertEqual(value.noncomplex().toarray().tolist(), [1, 4, 2, 5, 3, 6])

        value.reshape((3, 2))
        self.assertEqual(value.size, (3, 2))
        self.assertEqual(list(value), [[1, 5], [4, 3], [2, 6]])

    def test_array_vector_constructor_and_clone_are_matlab_like(self):
        import nelson

        value = nelson.double(vector=[11, 22, 33])
        copied = value.clone()
        copied[0] = [1, 2, 3]

        self.assertEqual(value.size, (1, 3))
        self.assertEqual(list(value), [[11.0, 22.0, 33.0]])
        self.assertEqual(list(copied), [[1.0, 2.0, 3.0]])

    def test_logical_rejects_complex_values(self):
        import nelson

        with self.assertRaisesRegex(TypeError, "logical arrays cannot be complex"):
            nelson.logical([True], is_complex=True)

    def test_complex_arrays_keep_shape_and_json_parts(self):
        import nelson

        value = nelson.double([[1 + 2j, 3 - 4j]], is_complex=True)
        payload = value._to_engine_json()

        self.assertEqual(value.size, (1, 2))
        self.assertEqual(payload["data"]["real"], [1.0, 3.0])
        self.assertEqual(payload["data"]["imag"], [2.0, -4.0])
        self.assertEqual(list(value.real()), [[1.0, 3.0]])
        self.assertEqual(list(value.imag()), [[2.0, -4.0]])

    def test_sparse_arrays_round_trip_through_engine_json(self):
        import nelson
        from nelson.engine import _from_engine_json

        value = nelson.sparse([0, 1], [1, 0], [5.0, 7.0], (2, 2))
        payload = value._to_engine_json()
        restored = _from_engine_json(payload)

        self.assertEqual(payload["is_sparse"], True)
        self.assertEqual(restored.nnz, 2)
        self.assertEqual(list(restored.todense()), [[0.0, 5.0], [7.0, 0.0]])

    def test_numpy_arrays_preserve_dtype_when_available(self):
        np = _import_or_install("numpy")
        from nelson.engine import _to_engine_json

        self.assertEqual(_to_engine_json(np.array([[True, False]]))["class"], "logical")
        self.assertEqual(_to_engine_json(np.array([[1, 2]], dtype=np.int16))["class"], "int16")

        payload = _to_engine_json(np.array([[1 + 2j]], dtype=np.complex128))
        self.assertEqual(payload["class"], "double")
        self.assertEqual(payload["data"]["imag"], [2.0])

    def test_dictionary_behaves_like_mapping(self):
        import nelson

        value = nelson.dictionary({"milk": 3.5})
        value["bread"] = 2.5

        self.assertEqual(len(value), 2)
        self.assertEqual(dict(value), {"milk": 3.5, "bread": 2.5})

    def test_struct_cell_and_table_engine_json(self):
        import nelson
        from nelson.engine import _from_engine_json, _to_engine_json

        struct_payload = _to_engine_json({"name": "Ada", "score": 42.0})
        self.assertEqual(struct_payload["class"], "struct")
        restored_struct = _from_engine_json(struct_payload)
        self.assertEqual(restored_struct["name"], "Ada")
        self.assertEqual(list(restored_struct["score"]), [[42.0]])

        value = nelson.cell([["Ada", 42.0]])
        restored_cell = _from_engine_json(value._to_engine_json())
        self.assertEqual(restored_cell[0][0], "Ada")
        self.assertEqual(list(restored_cell[0][1]), [[42.0]])

        table_payload = nelson.table({"Age": nelson.double(vector=[36, 41])})._to_engine_json()
        restored_table = _from_engine_json(table_payload)
        if hasattr(restored_table, "columns") and not isinstance(restored_table, nelson.table):
            self.assertEqual(list(restored_table["Age"]), [36.0, 41.0])
        else:
            self.assertEqual(list(restored_table["Age"]), [[36.0, 41.0]])

    def test_invalid_raw_dict_transfer_is_explicit(self):
        from nelson.engine import _to_engine_json

        with self.assertRaisesRegex(TypeError, "valid Nelson struct field names"):
            _to_engine_json({"not valid": 10})

        with self.assertRaisesRegex(TypeError, "non-scalar struct array"):
            _to_engine_json([{"name": "Ada"}, {"name": "Grace"}])

    def test_pandas_dataframe_converts_to_table_when_available(self):
        pd = _import_or_install("pandas")
        from nelson.engine import _to_engine_json

        payload = _to_engine_json(pd.DataFrame({"Age": [36, 41]}))
        self.assertEqual(payload["class"], "table")
        self.assertEqual(payload["variables"], ["Age"])
        self.assertEqual(payload["row_names"], [])

        dates = pd.date_range("2026-01-01", periods=2, freq="D")
        datetime_payload = _to_engine_json(pd.DataFrame({"Age": [36, 41]}, index=dates))
        self.assertEqual(datetime_payload["class"], "table")
        self.assertEqual(datetime_payload["row_names"], [str(item) for item in dates])

    def test_public_workspace_accepts_numpy_without_private_helpers(self):
        np = _import_or_install("numpy")
        from nelson.engine import NelsonEngine

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                return ""

            def put(self, handle, name, value):
                import nelson.engine as engine_module

                self.values[name] = engine_module._to_engine_json(value)

            def get(self, handle, name):
                return self.values[name]

            def close(self, handle):
                pass

        engine = NelsonEngine(123, backend=Backend())
        engine.workspace["x"] = np.array([[1, 2]], dtype=np.int16)
        engine.workspace["z"] = np.array([[1 + 2j]], dtype=np.complex128)

        self.assertEqual(engine._backend.values["x"]["class"], "int16")
        self.assertEqual(engine._backend.values["z"]["class"], "double")
        self.assertEqual(engine._backend.values["z"]["data"]["imag"], [2.0])

    def test_public_workspace_accepts_pandas_without_private_helpers(self):
        pd = _import_or_install("pandas")
        from nelson.engine import NelsonEngine

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                return ""

            def put(self, handle, name, value):
                import nelson.engine as engine_module

                self.values[name] = engine_module._to_engine_json(value)

            def get(self, handle, name):
                return self.values[name]

            def close(self, handle):
                pass

        engine = NelsonEngine(123, backend=Backend())
        engine.workspace["patients"] = pd.DataFrame({"Age": [36, 41], "Smoker": [True, False]})

        self.assertTrue(any("table(" in command for command in engine._backend.commands))
        payloads = list(engine._backend.values.values())
        self.assertEqual(payloads[0]["class"], "int64")
        self.assertEqual(payloads[1]["class"], "logical")

        engine._backend.values.clear()
        engine._backend.commands.clear()
        dates = pd.date_range("2026-01-01", periods=2, freq="D")
        engine.workspace["dated"] = pd.DataFrame({"Age": [36, 41]}, index=dates)
        self.assertTrue(any("'RowNames'" in command for command in engine._backend.commands))
        row_name_payloads = [
            payload for payload in engine._backend.values.values()
            if payload["class"] == "cell"
        ]
        self.assertEqual(row_name_payloads[0]["data"][0]["data"], str(dates[0]))

    def test_unsupported_object_transfer_is_explicit(self):
        from nelson.engine import _to_engine_json

        class Unsupported:
            pass

        with self.assertRaisesRegex(TypeError, "Unsupported value"):
            _to_engine_json(Unsupported())

    def test_future_result_wraps_future(self):
        from nelson.engine import FutureResult

        future = Future()
        wrapped = FutureResult(future)
        self.assertFalse(wrapped.done())
        future.set_result(42)
        self.assertTrue(wrapped.done())
        self.assertEqual(wrapped.result(), 42)

    def test_future_result_timeout_cancel_and_exception_paths(self):
        from nelson.engine import CancelledError, FutureResult, NelsonExecutionError, TimeoutError

        with ThreadPoolExecutor(max_workers=1) as executor:
            wrapped = FutureResult(executor.submit(lambda: (time.sleep(0.2), 99)[1]))
            with self.assertRaises(TimeoutError):
                wrapped.result(timeout=0.001)
            self.assertEqual(wrapped.result(timeout=2), 99)

        future = Future()
        wrapped = FutureResult(future)
        self.assertTrue(wrapped.cancel())
        self.assertTrue(wrapped.cancelled())
        with self.assertRaises(CancelledError):
            wrapped.result()

        future = Future()
        wrapped = FutureResult(future)
        future.set_exception(NelsonExecutionError("boom"))
        with self.assertRaisesRegex(NelsonExecutionError, "boom"):
            wrapped.result()

    def test_eval_routes_error_text_to_stderr_and_raises(self):
        from nelson.engine import NelsonEngine, NelsonExecutionError

        class Backend:
            def eval(self, handle, command):
                return "\nError: \nboom\n"

            def close(self, handle):
                self.closed = handle

        engine = NelsonEngine(123, backend=Backend())
        stderr = io.StringIO()
        with self.assertRaisesRegex(NelsonExecutionError, "boom"):
            engine.eval("bad()", stderr=stderr, nargout=0)
        self.assertIn("boom", stderr.getvalue())

    def test_engine_eval_workspace_and_dynamic_dispatch_use_backend(self):
        import nelson
        from nelson.engine import NelsonEngine

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                if command.startswith("["):
                    self.values["__out__"] = nelson.double([[4]])
                if "issparse(" in command:
                    self.values["__issparse__"] = nelson.logical([["issparse(s)" in command]])
                if "istable(" in command:
                    self.values["__istable__"] = nelson.logical([[False]])
                if "isstruct(" in command:
                    self.values["__isstruct__"] = nelson.logical([[False]])
                if "isa(" in command and "'dictionary'" in command:
                    self.values["__isdict__"] = nelson.logical([["isa(d," in command]])
                if "dictionary(" in command:
                    self.values["d"] = nelson.dictionary({"not valid": nelson.double([[3]])})
                if "keys(d)" in command:
                    self.values["__dict_keys__"] = nelson.cell([["not valid"]])
                    self.values["__dict_values__"] = nelson.cell([[nelson.double([[3]])]])
                if "full(s)" in command:
                    self.values["__full__"] = nelson.double([[0, 9], [0, 0]])
                    self.values["__size__"] = nelson.double(vector=[2, 2])
                    self.values["__logical__"] = nelson.logical([[False]])
                return "printed"

            def put(self, handle, name, value):
                if isinstance(value, (int, float)):
                    value = nelson.double([[value]])
                self.values[name] = value

            def get(self, handle, name):
                if name.endswith("_out_0"):
                    return self.values["__out__"]
                if name.endswith("_issparse"):
                    return self.values.get("__issparse__", nelson.logical([[False]]))
                if name.endswith("_istable"):
                    return self.values.get("__istable__", nelson.logical([[False]]))
                if name.endswith("_isstruct"):
                    return self.values.get("__isstruct__", nelson.logical([[False]]))
                if name.endswith("_isdict"):
                    return self.values.get("__isdict__", nelson.logical([[False]]))
                if name.endswith("_keys"):
                    return self.values["__dict_keys__"]
                if name.endswith("_values"):
                    return self.values["__dict_values__"]
                if name.endswith("_full"):
                    return self.values["__full__"]
                if name.endswith("_size"):
                    return self.values["__size__"]
                if name.endswith("_logical"):
                    return self.values["__logical__"]
                return self.values[name]

            def close(self, handle):
                self.closed = handle

        backend = Backend()
        engine = NelsonEngine(123, backend=backend)

        self.assertIsNone(engine.eval("a = 1", nargout=0))
        self.assertEqual(engine.eval("disp(a)", nargout=1), "printed")

        engine.workspace["x"] = nelson.double([[2]])
        self.assertEqual(list(engine.workspace["x"]), [[2.0]])

        engine.workspace["s"] = nelson.sparse([0], [1], [9.0], (2, 2))
        self.assertTrue(any("sparse(" in command for command in backend.commands))
        self.assertEqual(list(engine.workspace["s"].todense()), [[0.0, 9.0], [0.0, 0.0]])

        engine.workspace["d"] = {"not valid": 3.0}
        self.assertTrue(any("dictionary(" in command for command in backend.commands))
        self.assertEqual(list(engine.workspace["d"]["not valid"]), [[3.0]])

        result = engine.sqrt(4.0)
        self.assertEqual(list(result), [[4.0]])
        self.assertTrue(any("sqrt(" in command for command in backend.commands))

        future = engine.sqrt(9.0, background=True)
        self.assertEqual(list(future.result(timeout=5)), [[4.0]])

    def test_medium_style_engine_workflow_uses_nelson_names(self):
        import nelson
        from nelson.engine import NelsonEngine

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                return ""

            def put(self, handle, name, value):
                if isinstance(value, (int, float)):
                    value = nelson.double([[value]])
                self.values[name] = value

            def get(self, handle, name):
                return self.values[name]

            def close(self, handle):
                self.closed = handle

        backend = Backend()
        engine = NelsonEngine(123, backend=backend)
        param_dict = nelson.dictionary({"param1": 10, "param2": 12})
        param_list = [1, 2, 3]

        self.assertIsNone(engine.myFunc(param_dict, param_list, nargout=0))
        engine.quit()

        self.assertTrue(any("myFunc(" in command for command in backend.commands))
        self.assertFalse(hasattr(importlib.import_module("nelson.engine"), "start_matlab"))
        self.assertEqual(backend.closed, 123)

    def test_scipioni_style_workspace_arrays_and_multi_output(self):
        import nelson
        from nelson.engine import NelsonEngine

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                if "issparse(" in command:
                    self.values["__issparse__"] = nelson.logical([[False]])
                if "gcd(" in command:
                    for index, value in enumerate((20.0, 1.0, -1.0)):
                        self.values[f"__out_{index}"] = nelson.double([[value]])
                if "power(" in command:
                    self.values["__out_0"] = nelson.double([[1.0, 4.0, 9.0, 16.0]])
                return ""

            def put(self, handle, name, value):
                if isinstance(value, (int, float)):
                    value = nelson.double([[value]])
                self.values[name] = value

            def get(self, handle, name):
                if name.endswith("_issparse"):
                    return self.values["__issparse__"]
                for index in range(3):
                    if name.endswith(f"_out_{index}"):
                        return self.values[f"__out_{index}"]
                return self.values[name]

            def close(self, handle):
                self.closed = handle

        backend = Backend()
        engine = NelsonEngine(123, backend=backend)
        engine.workspace["y"] = 4.0

        self.assertEqual(list(engine.workspace["y"]), [[4.0]])
        self.assertEqual(tuple(list(v)[0][0] for v in engine.gcd(100.0, 80.0, nargout=3)), (20.0, 1.0, -1.0))

        base = nelson.double([1.0, 2.0, 3.0, 4.0])
        powered = engine.power(base, 2.0)
        self.assertEqual(list(powered), [[1.0, 4.0, 9.0, 16.0]])

    def test_sort_and_plot_patient_data_workflow(self):
        import nelson
        from nelson.engine import NelsonEngine

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                if "issparse(" in command:
                    self.values["__issparse__"] = nelson.logical([[False]])
                if "mean(" in command:
                    arg_name = command[command.find("(") + 1:command.find(")")]
                    data = self.values[arg_name]._data
                    self.values["__out_0"] = nelson.double([[sum(data) / len(data)]])
                if "linspace(" in command:
                    self.values["__out_0"] = nelson.double([[1.0, 2.0]])
                return ""

            def put(self, handle, name, value):
                if isinstance(value, (int, float)):
                    value = nelson.double([[value]])
                self.values[name] = value

            def get(self, handle, name):
                if name.endswith("_issparse"):
                    return self.values["__issparse__"]
                if name.endswith("_out_0"):
                    return self.values["__out_0"]
                return self.values[name]

            def close(self, handle):
                self.closed = handle

        backend = Backend()
        eng = NelsonEngine(123, backend=backend)
        patient_data = {
            "Diastolic": nelson.double(vector=[82.0, 76.0, 91.0, 79.0]),
            "Smoker": nelson.logical(vector=[True, False, True, False]),
        }
        pressure = patient_data["Diastolic"][0]
        smoker = patient_data["Smoker"][0]

        smokers = [p for p, s in zip(pressure, smoker) if s is True]
        nonsmokers = [p for p, s in zip(pressure, smoker) if s is False]

        sp = nelson.double(smokers)
        nsp = nelson.double(nonsmokers)
        self.assertEqual(list(eng.mean(sp)), [[86.5]])
        self.assertEqual(list(eng.mean(nsp)), [[77.5]])

        sdx = eng.linspace(1.0, float(len(smokers)), len(smokers))
        nsdx = eng.linspace(1.0, float(len(nonsmokers)), len(nonsmokers))
        self.assertEqual(list(sdx), [[1.0, 2.0]])
        self.assertEqual(list(nsdx), [[1.0, 2.0]])

        self.assertIsNone(eng.figure(nargout=0))
        self.assertIsNone(eng.hold("on", nargout=0))
        self.assertIsNone(eng.box("on", nargout=0))
        self.assertIsNone(eng.scatter(sdx, sp, 10.0, "blue", nargout=0))
        self.assertIsNone(eng.scatter(nsdx, nsp, 10.0, "red", nargout=0))
        self.assertIsNone(eng.xlabel("Patient (Anonymized)", nargout=0))
        self.assertIsNone(eng.ylabel("Diastolic Blood Pressure", nargout=0))
        self.assertIsNone(eng.title("Blood Pressure Readings", nargout=0))
        self.assertIsNone(eng.legend("Smokers", "Nonsmokers", nargout=0))
        self.assertIsNone(eng.line(nelson.double([0, 3]), nelson.double([86.5, 86.5]), "Color", "blue", nargout=0))

        self.assertTrue(any("scatter(" in command for command in backend.commands))
        self.assertTrue(any("legend(" in command for command in backend.commands))

    def test_nelson_object_handles_are_passed_back_by_workspace_reference(self):
        import nelson
        from nelson.engine import NelsonEngine, NelsonObject

        class Backend:
            def __init__(self):
                self.values = {"h": NelsonObject("graphics_object", {"id": 1})}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                if "isgraphics(" in command:
                    flag_name = command.split(" = false", 1)[0].strip()
                    target = command[command.find("isgraphics(") + len("isgraphics("):command.find("); catch")]
                    self.values[flag_name] = nelson.logical([[target in {"h"} or target.startswith("__nelson_py_handle_")]])
                    class_name = command[command.rfind(", ") + 2:command.rfind(" = class")]
                    if self.values[flag_name]._data[0]:
                        self.values[class_name] = nelson.char("graphics_object")
                if command.startswith("__nelson_py_handle_") and " = h;" in command:
                    alias = command.split("=", 1)[0].strip()
                    self.values[alias] = self.values["h"]
                if " = __nelson_py_handle_" in command:
                    alias, source = command.split("=", 1)
                    self.values[alias.strip()] = self.values[source.strip(" ;")]
                if " = get(" in command:
                    out_name = command[1:command.find("]")]
                    self.values[out_name] = nelson.char("on")
                return ""

            def put(self, handle, name, value):
                self.values[name] = value

            def get(self, handle, name):
                return self.values[name]

            def close(self, handle):
                self.closed = handle

        backend = Backend()
        engine = NelsonEngine(123, backend=backend)

        handle = engine.workspace["h"]
        self.assertIsInstance(handle, NelsonObject)
        self.assertEqual(handle.class_name, "graphics_object")
        self.assertIn("workspace", handle.payload)

        engine.figure(handle, nargout=0)
        self.assertTrue(any("figure(" in command for command in backend.commands))
        self.assertTrue(any(" = __nelson_py_handle_" in command for command in backend.commands))

        self.assertEqual(str(handle.get("Visible")), "on")
        self.assertIsNone(handle.set("Visible", "off"))
        self.assertTrue(any("get(" in command for command in backend.commands))
        self.assertTrue(any("set(" in command for command in backend.commands))

        handle.release()
        self.assertFalse(engine._object_handle_names)

    def test_non_scalar_struct_returns_object_handle(self):
        import nelson
        from nelson.engine import NelsonEngine, NelsonObject

        class Backend:
            def __init__(self):
                self.values = {}
                self.commands = []

            def eval(self, handle, command):
                self.commands.append(command)
                if "isstruct(" in command:
                    flag_name = command.split(" = ", 1)[0].strip()
                    self.values[flag_name] = nelson.logical([[True]])
                if "numel(s)" in command:
                    size_name = command.split(" = ", 1)[0].strip()
                    self.values[size_name] = nelson.double([[2]])
                if command.startswith("__nelson_py_handle_") and " = s;" in command:
                    alias = command.split("=", 1)[0].strip()
                    self.values[alias] = "struct-array"
                return ""

            def put(self, handle, name, value):
                self.values[name] = value

            def get(self, handle, name):
                return self.values[name]

            def close(self, handle):
                self.closed = handle

        engine = NelsonEngine(123, backend=Backend())
        value = engine.workspace["s"]
        self.assertIsInstance(value, NelsonObject)
        self.assertEqual(value.class_name, "struct")


class NelsonEngineLiveIntegrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if os.environ.get("NELSON_ENGINE_RUN_LIVE_TESTS") != "1":
            raise unittest.SkipTest("set NELSON_ENGINE_RUN_LIVE_TESTS=1 to run live Nelson engine tests")
        cls.dll = Path(__file__).resolve().parents[3] / "bin" / "x64" / "libnlsEngine.dll"
        if not cls.dll.exists():
            raise unittest.SkipTest("Nelson engine DLL is not available")
        os.environ.setdefault("NELSON_ENGINE_LIBRARY", str(cls.dll))
        os.environ["PATH"] = str(cls.dll.parent) + os.pathsep + os.environ.get("PATH", "")

    def test_user_script_function_cd_addpath_and_genpath(self):
        import nelson.engine

        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            nested = root / "nested"
            nested.mkdir()
            (root / "triarea_script.m").write_text(
                "b = 5;\nh = 3;\na = 0.5 * (b .* h)\n", encoding="utf-8"
            )
            (root / "triarea_fun.m").write_text(
                "function a = triarea_fun(b, h)\n"
                "  a = 0.5 * (b .* h);\n"
                "end\n",
                encoding="utf-8",
            )
            (nested / "path_marker.m").write_text(
                "function path_marker()\n"
                "  disp('path marker')\n"
                "end\n",
                encoding="utf-8",
            )

            eng = nelson.engine.start_nelson()
            try:
                eng.cd(str(root), nargout=0)
                self.assertIsNone(eng.triarea_script(nargout=0))
                self.assertEqual(list(eng.workspace["a"]), [[7.5]])

                result = eng.triarea_fun(1.0, 5.0)
                self.assertEqual(list(result), [[2.5]])

                eng.addpath(eng.genpath(str(root)), nargout=0)
                self.assertIsNone(eng.path_marker(nargout=0))
            finally:
                eng.quit()

    def test_live_workspace_conversions_and_async(self):
        import nelson
        import nelson.engine

        np = _import_or_install("numpy")
        pd = _import_or_install("pandas")

        eng = nelson.engine.start_nelson()
        try:
            eng.eval("a = 1 + 2;", nargout=0)
            self.assertEqual(list(eng.workspace["a"]), [[3.0]])

            eng.workspace["m"] = nelson.double([[1, 2], [3, 4]])
            self.assertEqual(list(eng.workspace["m"]), [[1.0, 2.0], [3.0, 4.0]])

            eng.workspace["l"] = nelson.logical([[True, False], [False, True]])
            self.assertEqual(list(eng.workspace["l"]), [[True, False], [False, True]])

            eng.workspace["z"] = np.array([[1 + 2j, 3 - 4j]], dtype=np.complex128)
            z = eng.workspace["z"]
            self.assertTrue(z.is_complex)
            self.assertEqual(list(z), [[1 + 2j, 3 - 4j]])

            eng.workspace["s"] = nelson.sparse([0, 1], [1, 0], [5.0, 7.0], (2, 2))
            self.assertEqual(list(eng.workspace["s"].todense()), [[0.0, 5.0], [7.0, 0.0]])

            eng.workspace["st"] = {"name": "Ada", "score": 42.0}
            st = eng.workspace["st"]
            self.assertEqual(st["name"], "Ada")
            self.assertEqual(list(st["score"]), [[42.0]])

            eng.workspace["cl"] = nelson.cell([["Ada", 42.0]])
            cl = eng.workspace["cl"]
            self.assertEqual(cl[0][0], "Ada")
            self.assertEqual(list(cl[0][1]), [[42.0]])

            eng.workspace["tb"] = nelson.table(
                {"Age": nelson.double(vector=[36, 41]), "Smoker": nelson.logical(vector=[True, False])}
            )
            tb = eng.workspace["tb"]
            self.assertEqual(getattr(tb, "shape", None), (2, 2))
            self.assertEqual(list(tb["Age"]), [36.0, 41.0])

            eng.workspace["df"] = pd.DataFrame({"Age": [36, 41], "Smoker": [True, False]})
            df = eng.workspace["df"]
            self.assertEqual(getattr(df, "shape", None), (2, 2))
            self.assertEqual(list(df["Age"]), [36, 41])

            dates = pd.date_range("2026-01-01", periods=2, freq="D")
            eng.workspace["dtf"] = pd.DataFrame({"Age": [36, 41]}, index=dates)
            dtf = eng.workspace["dtf"]
            self.assertEqual(list(dtf["Age"]), [36, 41])
            self.assertEqual(list(dtf.index), [str(item) for item in dates])

            eng.workspace["dv"] = {"not valid": 3.0}
            dv = eng.workspace["dv"]
            self.assertEqual(list(dv["not valid"]), [[3.0]])

            self.assertEqual(list(eng.sqrt(9.0, background=True).result(timeout=30)), [[3.0]])
        finally:
            eng.quit()

    def test_live_graphics_handle_pass_through(self):
        import nelson.engine
        from nelson.engine import NelsonObject

        eng = nelson.engine.start_nelson()
        handle = None
        try:
            handle = eng.figure(nargout=1)
            self.assertIsInstance(handle, NelsonObject)
            self.assertTrue(handle.class_name)
            self.assertIsNone(eng.figure(handle, nargout=0))
            try:
                previous_visible = handle.get("Visible")
                self.assertTrue(str(previous_visible))
                self.assertIsNone(handle.set("Visible", previous_visible))
            except Exception:
                pass
            self.assertIsNone(eng.feval("close", handle, nargout=0))
        finally:
            if handle is not None:
                try:
                    handle.release()
                except Exception:
                    pass
            eng.quit()

    def test_live_two_independent_engines(self):
        import nelson.engine

        first = nelson.engine.start_nelson()
        second = nelson.engine.start_nelson()
        try:
            first.workspace["a"] = 10.0
            second.workspace["a"] = 20.0
            self.assertEqual(list(first.workspace["a"]), [[10.0]])
            self.assertEqual(list(second.workspace["a"]), [[20.0]])
        finally:
            first.quit()
            second.quit()

    def test_live_find_connect_and_detach(self):
        import nelson.engine

        owner = nelson.engine.start_nelson()
        connected = None
        try:
            pids = nelson.engine.find_nelson()
            self.assertTrue(pids)
            connected = nelson.engine.connect_nelson()
            connected.workspace["connected_value"] = 123.0
            self.assertEqual(list(owner.workspace["connected_value"]), [[123.0]])
            connected.quit()
            connected = None
            self.assertEqual(list(owner.workspace["connected_value"]), [[123.0]])
        finally:
            if connected is not None:
                connected.quit()
            owner.quit()

    def test_live_stdout_and_stderr_file_like_objects(self):
        import nelson.engine
        from nelson.engine import NelsonExecutionError

        eng = nelson.engine.start_nelson()
        try:
            stdout = io.StringIO()
            self.assertIsNone(eng.eval("disp('hello from nelson')", stdout=stdout, nargout=0))
            self.assertIn("hello from nelson", stdout.getvalue())

            stderr = io.StringIO()
            with self.assertRaises(NelsonExecutionError):
                eng.eval("error('python engine stderr smoke')", stderr=stderr, nargout=0)
            self.assertTrue(stderr.getvalue())
        finally:
            eng.quit()


if __name__ == "__main__":
    unittest.main()
