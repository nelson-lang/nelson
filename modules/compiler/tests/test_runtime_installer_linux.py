#!/usr/bin/env python3
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
"""Exercise the native Linux installer without loading a numerical runtime."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time
import unittest


def sha(value):
    return hashlib.sha256(value).hexdigest()


class InstallerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        temporary = tempfile.TemporaryDirectory(prefix="checkpoint-", dir=cls.work)
        cls.addClassCleanup(temporary.cleanup)
        cls.checkpoint = str(Path(temporary.name) / "checkpoint.so")
        source = Path(__file__).parent / "fixtures/runtime_installer/checkpoint.c"
        subprocess.run(["cc", "-shared", "-fPIC", "-Wall", "-Wextra", "-Werror",
                        str(source), "-o", cls.checkpoint, "-ldl"], check=True, timeout=30)

    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="runtime-", dir=self.work)
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.destination = self.root / "installed runtime ' \u6f22\u5b57"
        self.engine = sha(b"synthetic interpreter, never load")
        self.registration = self.root / "config/nelson/runtimes/glnxa64" / (self.engine + ".root")

    def call(self, *args, success=True):
        result = subprocess.run([self.tool, *map(str, args)], capture_output=True, text=True, timeout=45)
        self.assertEqual(result.returncode == 0, success, result.stdout + result.stderr)
        return result

    def package(self, name, additional=False, count=0):
        root = self.root / name
        payload = root / "payload"
        payload.mkdir(parents=True)
        (payload / "modules").mkdir()
        data = {"bin/linux/libnlsInterpreter.so": b"synthetic interpreter, never load", "data/first.txt": b"first"}
        if additional:
            data["data/second.txt"] = b"second"
        for index in range(count):
            data[f"extra/{index:04d}.bin"] = bytes([index % 256]) * (256 * 1024)
        files = []
        for relative, value in data.items():
            filename = payload / relative
            filename.parent.mkdir(parents=True, exist_ok=True)
            filename.write_bytes(value)
            files.append(dict(relative=relative, sha256=sha(value), mode=420))
        names = ["core", "engine", "interpreter", "trigonometric_functions"]
        inventory = dict(format=1, kind="nelson-runtime-inventory", architecture="glnxa64",
                         runtimeVersion="2.0.0", engineFingerprint=self.engine, startupOrder=names,
                         moduleOrder=names if additional else names[:3],
                         flags=dict(gui=False, dynamicStartup=False), files=files, directories=[])
        (root / "runtime-package.json").write_text(json.dumps(inventory), encoding="utf-8")
        shutil.copy2(self.tool, root / "runtime-installer")
        self.call("--prepare", root)
        return root

    def install(self, package, success=True, destination=None):
        return self.call("--install", package, destination or self.destination, self.registration, success=success)

    def test_union_idempotence_and_owned_removal(self):
        first, second = self.package("a"), self.package("b", additional=True)
        self.install(first)
        original = self.destination / "data/first.txt"
        os.utime(original, ns=(123000000000, 123000000000))
        self.install(second)
        self.install(first)
        self.assertEqual(original.stat().st_mtime_ns, 123000000000)
        manifest = json.loads((self.destination / "runtime.json").read_text())
        self.assertEqual(manifest["moduleOrder"], ["core", "engine", "interpreter", "trigonometric_functions"])
        startup = (self.destination / "modules/modules.m").read_bytes()
        self.assertEqual(manifest["generated"]["sha256"], sha(startup))
        shutil.rmtree(first)
        shutil.rmtree(second)
        original.write_text("modified")
        (self.destination / "user.txt").write_text("user")
        result = subprocess.run([str(self.destination / ".nelson-runtime/uninstall")], capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(original.read_text(), "modified")
        self.assertEqual((self.destination / "user.txt").read_text(), "user")
        self.assertFalse((self.destination / "data/second.txt").exists())
        self.assertFalse(self.registration.exists())

    def test_conflict_and_changed_existing_files(self):
        first, second = self.package("a"), self.package("b", True)
        self.install(first)
        ledger = self.destination / ".nelson-runtime/inventory.json"
        before = ledger.read_bytes()
        filename = self.destination / "data/first.txt"
        filename.write_text("changed")
        self.install(second, success=False)
        self.assertEqual(ledger.read_bytes(), before)
        self.assertFalse((self.destination / "data/second.txt").exists())
        filename.write_text("first")
        (self.destination / "data/second.txt").write_text("second")
        self.install(second, success=False)
        self.assertEqual(ledger.read_bytes(), before)

    def test_registration_and_unmanaged_destinations(self):
        package = self.package("a")
        self.destination.mkdir()
        sentinel = self.destination / "unmanaged"
        sentinel.write_text("keep")
        self.install(package, success=False)
        self.assertEqual(sentinel.read_text(), "keep")
        sentinel.unlink()
        self.install(package)
        other = self.root / "other"
        self.install(package, success=False, destination=other)
        self.assertFalse(other.exists())
        self.registration.write_text("changed")
        self.install(package, success=False)
        self.assertEqual(self.registration.read_text(), "changed")

    def test_redirected_and_special_files(self):
        first, second = self.package("a"), self.package("b", True)
        self.install(first)
        original = self.destination / "data/first.txt"
        original.unlink()
        os.mkfifo(original)
        self.install(second, success=False)
        original.unlink()
        outside = self.root / "outside"
        outside.write_text("first")
        original.symlink_to(outside)
        self.install(second, success=False)
        self.assertEqual(outside.read_text(), "first")

    def interrupt(self, package):
        process = subprocess.Popen([self.tool, "--install", str(package), str(self.destination), str(self.registration)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            deadline = time.monotonic() + 30
            first_added = self.destination / "extra/0000.bin"
            while not first_added.exists() and process.poll() is None and time.monotonic() < deadline:
                time.sleep(0.001)
            self.assertIsNone(process.poll(), "Installer finished before the interruption checkpoint")
            self.assertTrue(first_added.exists())
            process.kill()
            process.communicate(timeout=10)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate(timeout=10)

    def test_process_interruption_recovery(self):
        first, second = self.package("a"), self.package("b", True, count=80)
        self.install(first)
        self.interrupt(second)
        first_added = self.destination / "extra/0000.bin"
        journal = self.destination / ".nelson-runtime/update.json"
        self.assertTrue(journal.exists())
        self.install(first, success=False)
        self.call("--uninstall", self.destination, success=False)
        first_added.write_bytes(b"changed")
        self.install(second, success=False)
        self.assertEqual(first_added.read_bytes(), b"changed")
        first_added.write_bytes((second / "payload/extra/0000.bin").read_bytes())
        first_added.chmod(0o644)
        self.install(second)
        self.assertFalse(journal.exists())
        self.assertEqual((self.destination / "extra/0079.bin").stat().st_size, 256 * 1024)

    def test_interrupted_first_installation(self):
        package = self.package("a", True, count=80)
        self.interrupt(package)
        self.assertFalse(self.registration.exists())
        self.install(package)
        self.assertTrue(self.registration.exists())
        self.assertFalse((self.destination / ".nelson-runtime/update.json").exists())
        self.call("--uninstall", self.destination)

    def test_permission_modes_and_startup_conflicts(self):
        package = self.package("a")
        filename = package / "runtime-package.json"
        original = json.loads(filename.read_text())
        for mode in (420.5, -1, 0, 1024):
            with self.subTest(mode=mode):
                altered = json.loads(json.dumps(original))
                altered["files"][0]["mode"] = mode
                filename.write_text(json.dumps(altered))
                self.install(package, success=False)
                self.assertFalse(self.destination.exists())
        filename.write_text(json.dumps(original))
        self.install(package)
        altered = json.loads(json.dumps(original))
        altered["startupOrder"].reverse()
        altered["moduleOrder"].reverse()
        filename.write_text(json.dumps(altered))
        self.install(package, success=False)

    def test_invalid_inventory_and_lock(self):
        import fcntl
        package = self.package("a")
        filename = package / "runtime-package.json"
        inventory = json.loads(filename.read_text())
        inventory["files"][0]["relative"] = "../outside"
        filename.write_text(json.dumps(inventory))
        self.install(package, success=False)
        self.assertFalse(self.destination.exists())
        valid = self.package("valid")
        self.install(valid)
        with (self.destination / ".nelson-runtime/operation.lock").open("rb") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            self.install(valid, success=False)
            self.call("--uninstall", self.destination, success=False)

    def test_reserved_metadata_descendants(self):
        package = self.package("a")
        filename = package / "runtime-package.json"
        original = filename.read_text()
        for name in ("runtime.json/child", "modules/modules.m/child", ".nelson-runtime/child"):
            with self.subTest(name=name):
                altered = json.loads(original)
                altered["files"][1]["relative"] = name
                filename.write_text(json.dumps(altered))
                result = self.install(package, success=False)
                self.assertIn("collides with installer metadata", result.stderr)
                self.assertFalse(self.destination.exists())

    def interrupt_at(self, package, path, action="publish"):
        self.interrupt_command_at([self.tool, "--install", str(package), str(self.destination), str(self.registration)],
                                  path, action)

    def interrupt_command_at(self, command, path, action="remove", hit=1):
        environment = os.environ.copy()
        environment.update(LD_PRELOAD=self.checkpoint, NELSON_INSTALLER_TEST_PATH=str(path),
                           NELSON_INSTALLER_TEST_ACTION=action, NELSON_INSTALLER_TEST_HIT=str(hit))
        process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=environment)
        try:
            deadline = time.monotonic() + 15
            status = Path(f"/proc/{process.pid}/status")
            stopped = False
            while process.poll() is None and time.monotonic() < deadline:
                try:
                    stopped = any(line.startswith("State:") and "T" in line.split()[1]
                                  for line in status.read_text().splitlines())
                except FileNotFoundError:
                    break
                if stopped:
                    break
                time.sleep(0.002)
            self.assertTrue(stopped, f"Process did not stop at {action} occurrence {hit}: {path}")
        finally:
            if process.poll() is None:
                process.kill()
            process.communicate(timeout=10)

    def test_metadata_publication_recovery(self):
        first, second = self.package("a"), self.package("b", True)
        targets = [".nelson-runtime/update.json", "modules/modules.m", "runtime.json",
                   ".nelson-runtime/uninstall", ".nelson-runtime/inventory.json", "registration"]
        for updating in (False, True):
            for target in targets:
                if updating and target == "registration":
                    continue  # An existing matching registration is deliberately not rewritten.
                with self.subTest(updating=updating, target=target):
                    if updating:
                        self.install(first)
                    path = self.registration if target == "registration" else self.destination / target
                    self.interrupt_at(second, path)
                    journal = self.destination / ".nelson-runtime/update.json"
                    self.assertTrue(journal.exists())
                    self.install(first, success=False)
                    self.install(second)
                    self.assertFalse(journal.exists())
                    self.assertEqual((self.destination / "data/second.txt").read_text(), "second")
                    self.call("--uninstall", self.destination)
                    self.assertFalse(self.registration.exists())

    def test_completed_update_before_process_exit(self):
        first, second = self.package("a"), self.package("b", True)
        for updating in (False, True):
            with self.subTest(updating=updating):
                if updating:
                    self.install(first)
                journal = self.destination / ".nelson-runtime/update.json"
                self.interrupt_at(second, journal, "remove")
                self.assertFalse(journal.exists())
                self.assertTrue(self.registration.exists())
                self.install(first)
                self.assertEqual((self.destination / "data/second.txt").read_text(), "second")
                self.call("--uninstall", self.destination)

    def test_repeated_installation_recovery(self):
        first, second = self.package("a"), self.package("b", True)
        for updating in (False, True):
            with self.subTest(updating=updating):
                if updating:
                    self.install(first)
                self.interrupt_at(second, self.destination / "runtime.json")
                self.interrupt_at(second, self.destination / ".nelson-runtime/inventory.json")
                self.install(second)
                self.assertFalse((self.destination / ".nelson-runtime/update.json").exists())
                self.assertEqual((self.destination / "data/second.txt").read_text(), "second")
                self.installed_uninstall()

    def installed_uninstall(self):
        helper = self.destination / ".nelson-runtime/uninstall"
        result = subprocess.run([str(helper)], capture_output=True, text=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_uninstall_interruption_boundaries(self):
        package = self.package("a", True)
        targets = [(".nelson-runtime/remove.json", "publish", 1),
                   ("registration", "remove", 1),
                   ("bin/linux/libnlsInterpreter.so", "remove", 1),
                   ("data/first.txt", "remove", 1),
                   ("modules/modules.m", "remove", 1),
                   ("runtime.json", "remove", 1),
                   (".nelson-runtime/inventory.json", "remove", 1),
                   ("bin/linux", "remove", 1),
                   (".nelson-runtime/remove.json", "publish", 2),
                   (".nelson-runtime/uninstall", "remove", 1)]
        helper = self.destination / ".nelson-runtime/uninstall"
        journal = self.destination / ".nelson-runtime/remove.json"
        for target, action, hit in targets:
            with self.subTest(target=target, action=action, hit=hit):
                self.install(package)
                path = self.registration if target == "registration" else self.destination / target
                self.interrupt_command_at([str(helper)], path, action, hit)
                before = json.loads(journal.read_text())
                if before["phase"] == "pending":
                    result = self.install(package, success=False)
                    self.assertIn("Resume runtime uninstallation", result.stderr)
                if helper.exists():
                    self.installed_uninstall()
                else:
                    self.assertEqual(before["phase"], "complete")
                    self.call("--uninstall", self.destination)
                self.assertEqual(json.loads(journal.read_text())["phase"], "complete")
                self.assertFalse(self.registration.exists())
                self.assertFalse(helper.exists())
                self.assertFalse((self.destination / "data/second.txt").exists())
                self.assertFalse((self.destination / "runtime.json").exists())
                self.assertFalse((self.destination / ".nelson-runtime/inventory.json").exists())
        self.install(package)
        self.assertFalse(journal.exists())
        self.installed_uninstall()

    def test_repeated_uninstall_recovery_preserves_files(self):
        package = self.package("a", True)
        self.install(package)
        helper = self.destination / ".nelson-runtime/uninstall"
        user = self.destination / "user.txt"
        user.write_text("user")
        modified = self.destination / "data/first.txt"
        modified.write_text("modified")
        self.interrupt_command_at([str(helper)], self.destination / "bin/linux/libnlsInterpreter.so")
        self.interrupt_command_at([str(helper)], self.destination / "modules/modules.m")
        self.installed_uninstall()
        self.assertEqual(user.read_text(), "user")
        self.assertEqual(modified.read_text(), "modified")
        receipt = self.destination / ".nelson-runtime/remove.json"
        completed = receipt.read_bytes()
        recreated = self.destination / "data/second.txt"
        recreated.write_text("second")
        self.call("--uninstall", self.destination)
        self.assertEqual(recreated.read_text(), "second")
        self.install(package, success=False)
        self.assertEqual(receipt.read_bytes(), completed)
        self.assertEqual(user.read_text(), "user")

    def test_uninstall_rejects_changed_journal_and_metadata(self):
        package = self.package("a", True)
        self.install(package)
        helper = self.destination / ".nelson-runtime/uninstall"
        journal = self.destination / ".nelson-runtime/remove.json"
        self.interrupt_command_at([str(helper)], journal, "publish")
        original = journal.read_bytes()
        altered = json.loads(original)
        altered["phase"] = "complete"
        journal.write_text(json.dumps(altered))
        self.call("--uninstall", self.destination, success=False)
        self.install(package, success=False)
        self.assertTrue((self.destination / "data/second.txt").exists())
        journal.write_bytes(original)
        manifest = self.destination / "runtime.json"
        manifest_bytes = manifest.read_bytes()
        manifest.write_text("changed")
        self.call("--uninstall", self.destination, success=False)
        self.assertTrue((self.destination / "data/second.txt").exists())
        self.assertEqual(manifest.read_text(), "changed")
        manifest.write_bytes(manifest_bytes)
        self.installed_uninstall()

    def test_completed_removal_reinstall_interruption(self):
        package = self.package("a", True)
        helper = self.destination / ".nelson-runtime/uninstall"
        journal = self.destination / ".nelson-runtime/remove.json"
        for target in ("helper", "receipt"):
            with self.subTest(target=target):
                self.install(package)
                self.interrupt_command_at([str(helper)], journal, "publish", 2)
                path = helper if target == "helper" else journal
                self.interrupt_at(package, path, "remove")
                self.install(package)
                self.assertFalse(journal.exists())
                self.assertTrue((self.destination / "data/second.txt").exists())
                self.installed_uninstall()

    def test_runtime_destination_trailing_separators(self):
        package = self.package("a")
        for separator in ("/", "///"):
            with self.subTest(separator=separator):
                self.call("--install", package, str(self.destination) + separator, self.registration)
                self.assertEqual(self.registration.read_text(), f"NELSON_RUNTIME_ROOT_1\n{self.destination}\n")
                self.installed_uninstall()
                self.assertFalse(self.registration.exists())
                receipt = json.loads((self.destination / ".nelson-runtime/remove.json").read_text())
                self.assertEqual(receipt["root"], str(self.destination))

    def test_reinstall_new_identity_after_completed_removal(self):
        first, second = self.package("a"), self.package("b", True)
        self.install(first)
        self.installed_uninstall()
        filename = second / "runtime-package.json"
        inventory = json.loads(filename.read_text())
        content = b"new synthetic interpreter, never load"
        engine = sha(content)
        interpreter = "bin/linux/libnlsInterpreter.so"
        (second / "payload" / interpreter).write_bytes(content)
        inventory["engineFingerprint"] = engine
        inventory["runtimeVersion"] = "2.0.1"
        for item in inventory["files"]:
            if item["relative"] == interpreter:
                item["sha256"] = engine
        filename.write_text(json.dumps(inventory))
        self.call("--prepare", second)
        previous = self.registration
        self.registration = self.registration.parent / (engine + ".root")
        self.install(second)
        self.assertFalse(previous.exists())
        self.assertEqual((self.destination / interpreter).read_bytes(), content)
        self.installed_uninstall()
        self.assertFalse(self.registration.exists())

    def test_uninstall_preserves_changed_registration_and_links(self):
        package = self.package("a", True)
        self.install(package)
        outside = self.root / "outside"
        outside.write_text("first")
        redirected = self.destination / "data/first.txt"
        redirected.unlink()
        redirected.symlink_to(outside)
        protected = self.destination / "data/second.txt"
        protected.chmod(0o400)
        registration = "NELSON_RUNTIME_ROOT_1\n/another/runtime\n"
        self.registration.write_text(registration)
        self.installed_uninstall()
        self.assertEqual(outside.read_text(), "first")
        self.assertTrue(redirected.is_symlink())
        self.assertEqual(protected.read_text(), "second")
        self.assertEqual(self.registration.read_text(), registration)

    def test_uninstall_retries_failed_payload_removal(self):
        package = self.package("a", True)
        self.install(package)
        directory = self.destination / "data"
        directory.chmod(0o555)
        try:
            self.call("--uninstall", self.destination, success=False)
        finally:
            directory.chmod(0o755)
        journal = self.destination / ".nelson-runtime/remove.json"
        self.assertEqual(json.loads(journal.read_text())["phase"], "pending")
        self.assertTrue((directory / "second.txt").exists())
        self.assertTrue((self.destination / "runtime.json").exists())
        self.installed_uninstall()
        self.assertEqual(json.loads(journal.read_text())["phase"], "complete")
        self.assertFalse(directory.exists())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("tool", type=Path)
    parser.add_argument("work", type=Path)
    args = parser.parse_args()
    InstallerTests.tool = str(args.tool.resolve(strict=True))
    InstallerTests.work = str(args.work.resolve(strict=True))
    unittest.main(argv=[__file__])
