#!/usr/bin/env python3
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
"""Bounded Linux runtime registration lookup using the production C++ header."""

import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


FINGERPRINT = "a" * 64
HEADER = b"NELSON_RUNTIME_ROOT_1\n"


class RegistryTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="registry-", dir=self.work)
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.home = self.root / "home"
        self.user = self.root / "user config"
        self.system = self.root / "system config"
        self.env = dict(os.environ, HOME=str(self.home), XDG_CONFIG_HOME=str(self.user),
                        XDG_CONFIG_DIRS=str(self.system))

    def record(self, directory, root):
        filename = directory / "nelson/runtimes/glnxa64" / (FINGERPRINT + ".root")
        filename.parent.mkdir(parents=True, exist_ok=True)
        filename.write_bytes(HEADER + str(root).encode("utf-8") + b"\n")
        return filename

    def lookup(self, argument=FINGERPRINT):
        result = subprocess.run([self.probe, argument], env=self.env, check=True,
                                capture_output=True, text=True, encoding="utf-8", timeout=3)
        return result.stdout.splitlines()

    def test_order_and_duplicate_roots(self):
        first = self.root / "first runtime"
        second = self.root / "second runtime"
        self.record(self.user, first)
        self.record(self.system, second)
        self.assertEqual(self.lookup(), [str(first), str(second)])
        self.record(self.system, first)
        self.assertEqual(self.lookup(), [str(first)])

    def test_home_fallback_and_relative_variables(self):
        selected = self.root / "fallback"
        self.record(self.home / ".config", selected)
        for value in ("", "relative/config"):
            with self.subTest(value=value):
                self.env["XDG_CONFIG_HOME"] = value
                self.assertEqual(self.lookup(), [str(selected)])
        self.env.pop("XDG_CONFIG_HOME")
        self.assertEqual(self.lookup(), [str(selected)])
        self.env["HOME"] = "relative"
        self.assertEqual(self.lookup(), [])

    def test_directory_symlink_parent_is_not_lexically_collapsed(self):
        target = self.root / "target" / "child"
        target.mkdir(parents=True)
        link = self.root / "link"
        link.symlink_to(target, target_is_directory=True)
        directory = link / ".." / "configuration"
        selected = self.root / "runtime"
        self.env["XDG_CONFIG_HOME"] = str(directory)
        self.record(directory, selected)
        self.assertEqual(self.lookup(), [str(selected)])

    def test_directory_defaults_and_limit(self):
        self.env.pop("XDG_CONFIG_DIRS")
        self.assertEqual(self.lookup("--directories"), [str(self.user), "/etc/xdg"])
        directories = [self.root / str(index) for index in range(17)]
        self.env["XDG_CONFIG_DIRS"] = ":".join(map(str, directories))
        self.record(directories[15], self.root / "selected")
        self.record(directories[16], self.root / "ignored")
        self.assertEqual(self.lookup(), [str(self.root / "selected")])
        self.env["XDG_CONFIG_DIRS"] = "relative::" + str(self.system)
        self.record(self.system, self.root / "system")
        self.assertEqual(self.lookup(), [str(self.root / "system")])

    def test_invalid_fingerprints(self):
        self.record(self.user, self.root / "runtime")
        for value in ("", "../" * 22, "b" * 63, "A" * 64, "g" * 64, "a" * 65):
            with self.subTest(value=value):
                self.assertEqual(self.lookup(value), [])

    def test_invalid_records_are_ignored(self):
        filename = self.record(self.user, self.root / "user")
        fallback = self.root / "fallback"
        self.record(self.system, fallback)
        invalid = [b"", HEADER, HEADER + b"/path", HEADER + b"relative\n",
                   HEADER + b"/first\n/second\n", HEADER + b"/nul\0path\n",
                   HEADER + b"/delete\x7fpath\n", b"NELSON_RUNTIME_ROOT_2\n/path\n",
                   b"\xef\xbb\xbf" + HEADER + b"/path\n", b"x" * 32769]
        for value in invalid:
            with self.subTest(value=value[:60]):
                filename.write_bytes(value)
                self.assertEqual(self.lookup(), [str(fallback)])

    def test_nonregular_records_do_not_block(self):
        filename = self.record(self.user, self.root / "runtime")
        filename.unlink()
        os.mkfifo(filename)
        self.assertEqual(self.lookup(), [])
        filename.unlink()
        target = self.root / "record"
        target.write_bytes(HEADER + b"/untrusted\n")
        filename.symlink_to(target)
        self.assertEqual(self.lookup(), [])
        filename.unlink()
        filename.mkdir()
        self.assertEqual(self.lookup(), [])

    def test_oversized_environment(self):
        selected = self.root / "fallback"
        self.record(self.home / ".config", selected)
        self.env["XDG_CONFIG_HOME"] = "/" + "x" * 32769
        self.assertEqual(self.lookup(), [str(selected)])

    def test_paths_are_literal_utf8(self):
        self.env["XDG_CONFIG_HOME"] = str(self.root / "config-\u6f22\u5b57")
        selected = self.root / "runtime ' \u6f22\u5b57 $(touch REGISTRY_MARKER)"
        self.record(Path(self.env["XDG_CONFIG_HOME"]), selected)
        self.assertEqual(self.lookup(), [str(selected)])
        self.assertFalse((self.root / "REGISTRY_MARKER").exists())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("probe", type=Path)
    parser.add_argument("work", type=Path)
    args = parser.parse_args()
    if sys.platform != "linux":
        raise SystemExit("This fixture requires Linux.")
    RegistryTests.probe = str(args.probe.resolve(strict=True))
    RegistryTests.work = str(args.work.resolve(strict=True))
    unittest.main(argv=[sys.argv[0]])
