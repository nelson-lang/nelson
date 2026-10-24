#!/usr/bin/env python3
# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
"""Source-free Linux CLI deployment checks using the existing multi-file fixture."""

import argparse
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import tempfile
import zipfile


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def embedded_application(executable, mode):
    data = executable.read_bytes()
    require(data[:4] == b"\x7fELF", "Output is not a native ELF executable")
    require(len(data) >= 352, "Missing application trailer")
    trailer = data[-352:]
    require(trailer[:8] == b"NLSAPP01", "Missing application archive")
    version, flags, offset, length = struct.unpack_from("<IIQQ", trailer, 8)
    require(version == (2 if mode == "installed" else 1), "Wrong trailer version")
    require(flags == (2 if mode == "installed" else 0), "Wrong CLI runtime mode")
    require(0 < offset < len(data) - 352, "Invalid archive offset")
    require(offset + length == len(data) - 352, "Invalid archive length")
    archive = data[offset:offset + length]
    require(hashlib.sha256(archive).hexdigest().encode() == trailer[32:96],
            "Embedded archive digest differs")
    with zipfile.ZipFile(io.BytesIO(archive)) as payload:
        require(payload.testzip() is None, "Embedded ZIP checksum differs")
        names = payload.namelist()
        manifest = json.loads(payload.read("application.json"))
    for suffix in ("app_entry.nbc", "app_helper.nbc", "private/private_scale.nbc",
                   "+ncpkg/twice.nbc", "data.txt"):
        require(any(name.endswith("/" + suffix) for name in names),
                "Missing embedded dependency: " + suffix)
    require(not any(name.endswith(".m") for name in names), "Application source leaked")
    require(manifest["runtimeMode"] == mode, "Manifest runtime mode differs")
    return names


def checked_runtime(runtime):
    inventory = json.loads((runtime / "runtime.json").read_text(encoding="utf-8"))
    require("compiler" not in inventory["modules"], "Build module leaked into runtime")
    require(not (runtime / "modules/compiler").exists(), "Build module data leaked")
    expected = {"runtime.json"}
    generated = inventory["generated"]
    if isinstance(generated, dict):
        generated = [generated]
    for item in inventory["files"] + generated:
        require("source" not in item, "Build-host source path leaked")
        target = (runtime / item["relative"]).resolve()
        require(target.is_relative_to(runtime), "Inventory path escapes runtime")
        require(digest(target) == item["sha256"].lower(), "Runtime digest differs: " + str(target))
        expected.add(item["relative"])
    actual = {path.relative_to(runtime).as_posix() for path in runtime.rglob("*") if path.is_file()}
    require(actual == expected, "Runtime file inventory is not exact")
    for item in inventory["libraries"]:
        require(not item["filename"].startswith(("libnlsCompiler", "libQt", "libnlsGui")),
                "Unexpected build or graphical library in CLI runtime")
    return inventory


def run_checks(binaries):
    repo = Path(__file__).resolve().parents[3]
    temporary = repo / "temp"
    temporary.mkdir(exist_ok=True)
    root = Path(tempfile.mkdtemp(prefix="nelsonc-linux-", dir=temporary)).resolve()
    require(root.is_relative_to(temporary.resolve()), "Test root escapes repository temp")
    print("ARTIFACTS=" + str(root), flush=True)
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith("NELSON") and key not in ("LD_LIBRARY_PATH", "LD_PRELOAD")}
    extraction = root / "extraction"
    extraction.mkdir()
    environment.update(TMPDIR=str(extraction), TMP=str(extraction), TEMP=str(extraction),
                       NELSON_PREFERENCES_PATH=str(root / "preferences"),
                       NELSON_USERPATH=str(root), PATH="/usr/bin:/bin",
                       OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1")
    compiler = binaries / "nelsonc"
    require(compiler.is_file(), "Build nelsonc first")

    def invoke(label, executable, arguments, expected=0, extra=None):
        env = environment | (extra or {})
        result = subprocess.run([str(executable), *arguments], cwd=root, env=env,
                                capture_output=True, text=True, encoding="utf-8", timeout=180)
        (root / (label + ".stdout.log")).write_text(result.stdout, encoding="utf-8")
        (root / (label + ".stderr.log")).write_text(result.stderr, encoding="utf-8")
        require(result.returncode == expected if expected is not None else result.returncode != 0,
                label + " failed:\n" + result.stdout + result.stderr)
        require(not list(extraction.glob("nelsonc-*")), label + " left an extraction directory")
        return result.stdout

    require("usage: nelsonc" in invoke("help", compiler, ["--help"]), "Missing CLI help")
    source = root / "source"
    shutil.copytree(Path(__file__).parent / "fixtures/nelsonc", source)
    distribution = root / "distribution"
    distribution.mkdir()
    entry = source / "app_entry.m"
    resource = source / "data.txt"
    resource_text = resource.read_text(encoding="utf-8").strip()
    plans = {}
    for mode in ("bundled", "installed"):
        arguments = [str(entry), "--cli", "-a", str(resource), "--runtime", mode]
        plan = json.loads(invoke(mode + "-plan", compiler, arguments + ["--explain-link"]))
        require(plan["complete"] and not plan["runtime"]["gui"], "Incomplete CLI plan")
        require("compiler" not in plan["runtime"]["modules"], "Build module retained")
        executable = distribution / mode
        invoke(mode + "-build", compiler, arguments + ["-o", str(executable)])
        require(os.access(executable, os.X_OK), "Application lacks executable permission")
        plans[mode] = embedded_application(executable, mode)
    require(not (distribution / "installed.runtime").exists(), "Installed mode copied a runtime")
    checked_runtime((distribution / "bundled.runtime").resolve())
    # Only the test-owned copy is removed; the input repository is untouched.
    require(source.parent == root and not source.is_symlink(), "Unsafe copied source directory")
    shutil.rmtree(source)
    relocated = root / "relocated distribution"
    distribution.rename(relocated)
    (root / "invocation-input.txt").write_text("caller-input", encoding="utf-8")
    arguments = ["first", "", "with spaces", 'quote"inside', "caf\u00e9"]
    for mode in ("bundled", "installed"):
        extra = {"NELSONC_RUNTIME_ROOT": str(repo)} if mode == "installed" else {}
        executable = relocated / mode
        output = invoke(mode + "-run", executable, arguments, extra=extra)
        require("VALUE=18" in output and "RESOURCE=" + resource_text in output,
                "Packaged numerical or embedded resource result differs")
        require("INPUT=caller-input" in output, "Invocation-directory input differs")
        encoded = next((line[10:] for line in output.splitlines() if line.startswith("ARGUMENTS=")), "")
        require(json.loads(encoded) == arguments, "Application arguments differ")
        require((root / "function-output.txt").read_text(encoding="utf-8").strip() == str(root),
                "Persistent output was not written in invocation directory")
        invoke(mode + "-exit", executable, ["--exit-seven"], expected=7, extra=extra)
        failure = invoke(mode + "-error", executable, ["--fail"], expected=None, extra=extra)
        require("Deliberate application error" in failure, "Application error was not reported")
    invoke("installed-missing", relocated / "installed", [], expected=None,
           extra={"NELSONC_RUNTIME_ROOT": str(root / "absent-runtime")})
    runtime = (relocated / "bundled.runtime").resolve()
    for relative in ("etc/startup.m", "bin/linux/libnlsInterpreter_builtin.so"):
        missing = runtime / relative
        saved = root / (missing.name + ".saved")
        require(missing.is_file() and not missing.is_symlink(), "Expected copied runtime file")
        missing.rename(saved)
        try:
            for mode in ("bundled", "installed"):
                output = invoke(mode + "-missing-" + missing.name, relocated / mode, [], expected=1,
                                extra={"NELSONC_RUNTIME_ROOT": str(runtime)} if mode == "installed" else {})
                require("VALUE=" not in output, "Application ran after initialization failure")
        finally:
            saved.rename(missing)
    checked_runtime(runtime)
    report = {"status": "passed", "binaries": str(binaries), "embeddedFiles": plans}
    (root / "report.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print("PASS source-free relocated Linux CLI applications: bundled and installed", flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binaries", required=True, type=Path)
    args = parser.parse_args()
    require(sys.platform.startswith("linux"), "This integration harness requires Linux")
    run_checks(args.binaries.resolve())
