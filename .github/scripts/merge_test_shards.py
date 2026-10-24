#!/usr/bin/env python3
# ==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
# This file is part of Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# ==============================================================================
# Merges the per-shard test results of one platform into one XML and one JSON file.
# Usage: merge_test_shards.py <results directory>
# The shard files are removed once the merged files are written, they are kept on any error.
# ==============================================================================
import json
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

SHARD_PATTERN = re.compile(r"^(?P<base>tests_all-.+)-shard-\d+\.(?P<ext>xml|json)$")


def group_shards(results_dir):
    groups = {}
    for path in sorted(results_dir.iterdir()):
        match = SHARD_PATTERN.match(path.name)
        if match:
            key = (match.group("base"), match.group("ext"))
            groups.setdefault(key, []).append(path)
    return groups


def merge_xml(paths, output):
    # one <testsuite> per module: a module split across shards is merged by name
    merged_root = ET.Element("testsuites")
    suites = {}
    order = []
    for path in paths:
        for suite in ET.parse(path).getroot().findall("testsuite"):
            name = suite.get("name", "")
            if name not in suites:
                suites[name] = ET.SubElement(merged_root, "testsuite", {"name": name})
                order.append(name)
            for testcase in suite.findall("testcase"):
                suites[name].append(testcase)
    totals = {"tests": 0, "failures": 0, "errors": 0, "skipped": 0}
    total_time = 0.0
    for name in order:
        suite = suites[name]
        counts = {"tests": 0, "failures": 0, "errors": 0, "skipped": 0}
        suite_time = 0.0
        for testcase in suite.findall("testcase"):
            counts["tests"] += 1
            if testcase.find("failure") is not None:
                counts["failures"] += 1
            if testcase.find("error") is not None:
                counts["errors"] += 1
            if testcase.find("skipped") is not None:
                counts["skipped"] += 1
            try:
                suite_time += float(testcase.get("time", 0))
            except ValueError:
                pass
        for key, value in counts.items():
            suite.set(key, str(value))
            totals[key] += value
        suite.set("time", "%.3f" % suite_time)
        total_time += suite_time
    for key, value in totals.items():
        merged_root.set(key, str(value))
    merged_root.set("time", "%.3f" % total_time)
    ET.ElementTree(merged_root).write(output, encoding="utf-8", xml_declaration=True)


def merge_json(paths, output):
    merged = None
    for path in paths:
        with open(path, encoding="utf-8") as stream:
            data = json.load(stream)
        if merged is None:
            merged = data
            continue
        for key in ("disabled", "errors", "passed", "tests", "bench", "time"):
            if key in data:
                merged[key] = merged.get(key, 0) + data[key]
        merged["tests_list"].extend(data.get("tests_list", []))
    with open(output, "w", encoding="utf-8") as stream:
        json.dump(merged, stream, indent=4)


def main():
    if len(sys.argv) != 2:
        print("usage: merge_test_shards.py <results directory>", file=sys.stderr)
        return 2
    results_dir = Path(sys.argv[1])
    if not results_dir.is_dir():
        # the build was cancelled or skipped: no shard was downloaded, nothing to merge
        print("no results directory %s, nothing to merge" % results_dir)
        return 0
    groups = group_shards(results_dir)
    if not groups:
        print("no shard file found in %s" % results_dir, file=sys.stderr)
        return 1
    for (base, ext), paths in groups.items():
        output = results_dir / ("%s.%s" % (base, ext))
        if ext == "xml":
            merge_xml(paths, output)
        else:
            merge_json(paths, output)
        print("%s <- %s" % (output.name, ", ".join(path.name for path in paths)))
    for paths in groups.values():
        for path in paths:
            path.unlink()
    return 0


if __name__ == "__main__":
    sys.exit(main())
