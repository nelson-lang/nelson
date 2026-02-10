import re
from pathlib import Path

src = Path(r"d:\Developpements\Github\nelson-lang\nelson\modules\error_manager\src\include\PredefinedErrorMessages.hpp")
out = Path(r"d:\Developpements\Github\nelson-lang\nelson\modules\error_manager\src\include\PredefinedErrorMessages.hpp.processed")

macro_re = re.compile(r"^\s*#define\s+([A-Za-z0-9_]+)\b")

seen = set()
removed = []

with src.open('r', encoding='utf-8') as f, out.open('w', encoding='utf-8') as w:
    lines = f.readlines()
    i = 0
    while i < len(lines):
        line = lines[i]
        m = macro_re.match(line)
        if m:
            name = m.group(1)
            # capture full macro block (handle backslash line continuations)
            block_lines = [line]
            while block_lines[-1].rstrip().endswith('\\') and i+1 < len(lines):
                i += 1
                block_lines.append(lines[i])
            block = ''.join(block_lines)
            if name in seen:
                removed.append(block)
            else:
                seen.add(name)
                w.write(block)
            i += 1
            continue
        else:
            w.write(line)
            i += 1

    if removed:
        w.write('\n// ===== DUPLICATED MACROS MOVED HERE =====\n')
        for b in removed:
            w.write(b)

print(f"Processed {src} -> {out}. Duplicates moved: {len(removed)}")
