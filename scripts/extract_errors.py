#!/usr/bin/env python3
"""
Scan the repository for `raiseError(...)` calls, collect ERROR_* identifiers,
look up their messages in PredefinedErrorMessages.hpp and emit a JSON file
mapping IDs to messages.

Usage: python scripts/extract_errors.py [--header PATH] [--out PATH]
"""
import argparse
import json
import os
import re
from pathlib import Path


def find_raise_errors(root):
    """Return list of (id_string, macro_name) tuples found in raiseError calls.

    If the first argument is a string literal (optionally with an L prefix),
    use it as the key. Otherwise fall back to the ERROR_ macro name.
    """
    results = []
    pattern = re.compile(r"raiseError\s*\((.*?)\)", re.DOTALL)
    id_pattern = re.compile(r"\b(ERROR_[A-Z0-9_]+)\b")
    # match C++ string literal optionally prefixed by L, allow escaped chars
    str_re = re.compile(r'(?:L)?"((?:[^"\\\\]|\\\\.)*)"')

    for dirpath, dirnames, filenames in os.walk(root):
        if any(x in dirpath for x in (".git", "bin", "build", "node_modules")):
            continue
        for fname in filenames:
            if not fname.endswith(('.cpp', '.c', '.hpp', '.h', '.cc', '.cxx', '.c++')):
                continue
            path = os.path.join(dirpath, fname)
            try:
                text = Path(path).read_text(encoding='utf-8', errors='ignore')
            except Exception:
                continue
            for m in pattern.finditer(text):
                args = m.group(1)
                # capture one or more adjacent string literals at the start of args
                s = None
                parts = []
                last_end = None
                for mstr in str_re.finditer(args):
                    start, end = mstr.span()
                    if last_end is None:
                        # first string literal
                        parts.append(mstr.group(1))
                        last_end = end
                        continue
                    # check that between last_end and this start there's only whitespace
                    between = args[last_end:start]
                    if re.fullmatch(r"\s*", between):
                        parts.append(mstr.group(1))
                        last_end = end
                        continue
                    # not adjacent -> stop collecting
                    break
                if parts:
                    s = ''.join(parts)
                # find the first ERROR_ macro in the args, but avoid matching
                # occurrences inside the string literal we just collected.
                search_pos = last_end if last_end is not None else 0
                idm = id_pattern.search(args, pos=search_pos)
                macro = idm.group(1) if idm else None
                # choose key: string literal if present, otherwise macro
                key = s if s else (macro if macro else None)
                if key and macro:
                    results.append((key, macro))
                elif key:
                    results.append((key, None))
    # deduplicate while preserving order
    seen = set()
    uniq = []
    for key, macro in results:
        if key not in seen:
            seen.add(key)
            uniq.append((key, macro))
    return uniq


def parse_header(header_path):
    text = Path(header_path).read_text(encoding='utf-8', errors='ignore')
    # join continued lines ending with backslash so multi-line defines become single lines
    text = re.sub(r'\\\s*\n\s*', ' ', text)
    # capture defines like: #define ERROR_NAME _W(...) and extract any
    # contained C/C++ string literals (handles L"..." and adjacent literals)
    define_re = re.compile(r'#define\s+(ERROR_[A-Z0-9_]+)\s+_W\(\s*(.*?)\s*\)', re.DOTALL)
    str_re = re.compile(r'(?:L)?"((?:[^"\\]|\\.)*)"')
    def extract_string_parts(body):
        parts = [p.group(1) for p in str_re.finditer(body)]
        if parts:
            return parts
        # fallback manual scanner (handles odd cases where regex fails)
        parts = []
        i = 0
        L = len(body)
        while i < L:
            # find next quote or L"
            if body[i] == 'L' and i + 1 < L and body[i+1] == '"':
                start = i + 2
                i += 2
            elif body[i] == '"':
                start = i + 1
                i += 1
            else:
                i += 1
                continue
            # collect until an unescaped quote
            j = start
            buf = []
            while j < L:
                ch = body[j]
                if ch == '"':
                    # check if escaped
                    # count backslashes before this quote
                    k = j - 1
                    backslashes = 0
                    while k >= start and body[k] == '\\':
                        backslashes += 1
                        k -= 1
                    if backslashes % 2 == 0:
                        break
                buf.append(ch)
                j += 1
            parts.append(''.join(buf))
            i = j + 1
        return parts
    mapping = {}
    for m in define_re.finditer(text):
        body = m.group(2)
        parts = extract_string_parts(body)
        raw = ''.join(parts)
        try:
            decoded = bytes(raw, 'utf-8').decode('unicode_escape')
        except Exception:
            decoded = raw
        mapping[m.group(1)] = decoded
    return mapping


def parse_all_defines(root):
    """Scan the repository for #define ERROR_... _W("...") and return a mapping."""
    define_re = re.compile(r'#define\s+(ERROR_[A-Z0-9_]+)\s+_W\(\s*(.*?)\s*\)', re.DOTALL)
    str_re = re.compile(r'(?:L)?"((?:[^"\\]|\\.)*)"')
    def extract_string_parts(body):
        parts = [p.group(1) for p in str_re.finditer(body)]
        if parts:
            return parts
        parts = []
        i = 0
        L = len(body)
        while i < L:
            if body[i] == 'L' and i + 1 < L and body[i+1] == '"':
                start = i + 2
                i += 2
            elif body[i] == '"':
                start = i + 1
                i += 1
            else:
                i += 1
                continue
            j = start
            buf = []
            while j < L:
                ch = body[j]
                if ch == '"':
                    k = j - 1
                    backslashes = 0
                    while k >= start and body[k] == '\\':
                        backslashes += 1
                        k -= 1
                    if backslashes % 2 == 0:
                        break
                buf.append(ch)
                j += 1
            parts.append(''.join(buf))
            i = j + 1
        return parts
    mapping = {}
    for dirpath, dirnames, filenames in os.walk(root):
        if any(x in dirpath for x in ('.git', 'bin', 'build', 'node_modules')):
            continue
        for fname in filenames:
            if not fname.endswith(('.hpp', '.h', '.hh', '.c', '.cpp', '.cc', '.cxx')):
                continue
            path = os.path.join(dirpath, fname)
            try:
                text = Path(path).read_text(encoding='utf-8', errors='ignore')
            except Exception:
                continue
            # join continued lines
            text = re.sub(r'\\\s*\n\s*', ' ', text)
            for m in define_re.finditer(text):
                body = m.group(2)
                parts = extract_string_parts(body)
                raw = ''.join(parts)
                try:
                    decoded = bytes(raw, 'utf-8').decode('unicode_escape')
                except Exception:
                    decoded = raw
                mapping[m.group(1)] = decoded
    return mapping


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--root', default='.', help='repository root to scan')
    p.add_argument('--header', default='modules/error_manager/src/include/PredefinedErrorMessages.hpp')
    p.add_argument('--out', default='root/messages/nelson_en_US.json')
    args = p.parse_args()

    entries = find_raise_errors(args.root)
    print(f'Found {len(entries)} raiseError entries (showing up to 20):', entries[:20])

    header_path = Path(args.header)
    if not header_path.exists():
        print('Header file not found:', header_path)
        return
    # parse repository-wide defines first (fallback source)
    all_defs = parse_all_defines(args.root)
    # parse the canonical header and let its values override repo-wide ones
    messages = parse_header(header_path)
    # fill any missing macros from all_defs
    added = 0
    for k, v in all_defs.items():
        if k not in messages:
            messages[k] = v
            added += 1
    if added:
        print(f'Added {added} defines from repository-wide scan to messages mapping.')

    # Assemble results: prefer canonical header message, fallback to repo-wide defines
    result = {}
    empties = {}
    for key, macro in entries:
        # only include entries where the first argument was a string literal
        # i.e. key is the string and different from the macro name
        if macro is None or key == macro:
            continue
        # prefer message from header (messages contains header + merged repo defs),
        # but also check all_defs explicitly to be robust
        msg = ''
        if macro:
            msg = messages.get(macro) or ''
            if not msg:
                # fallback explicit check
                msg = ''
        msg = (msg or '').strip()
        if msg:
            result[key] = msg
        else:
            empties[key] = ''
    # append empties at the end to ease manual review
    result.update(empties)

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(result, ensure_ascii=False, indent=2))
    print('Wrote', out_path)


if __name__ == '__main__':
    main()
