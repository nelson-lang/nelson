---
name: nelson-usage-interfacing
audience: user
modules: python_engine, julia_engine, dynamic_link, webtools
description: Call external code and services from Nelson - C libraries, Python, the system shell, and the web. Use when the user wants to load a shared library, run Python, call a command, or fetch a URL.
triggers: bibliotheque, library, dll, so, dylib, c, dlopen, charger librairie, python, pyrun, appeler, call, externe, external, interface, systeme, system, commande, command, shell, web, http, url, webread, api, rest
functions: dlopen, dlcall, dlsym, pyrun, pyrunfile, pyenv, system, getenv, setenv, webread, webwrite, websave
---

# Interfacing with External Code and Services

Reach outside Nelson - a C library, Python, the shell, or the web - and bring the result back in.

## When to use
- Loading and calling a C/shared library.
- Running Python code or a Python file.
- Running a system command, or reading/writing over HTTP.

## When NOT to use
- Pure in-language computation (use the relevant Nelson guide).

## Key functions
C libraries: `dlopen`, `dlsym`, `dlcall`. Python: `pyrun`, `pyrunfile`, `pyenv`. Shell/env: `system`, `getenv`, `setenv`. Web: `webread`, `webwrite`, `websave`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. C library: `lib = dlopen(path)`, then call an exported symbol with `dlcall` (prototype the argument/return types); keep the handle until done.
2. Python: run a snippet with `pyrun` or a file with `pyrunfile`; `pyenv` selects/inspects the interpreter.
3. Shell: `[status, out] = system(cmd)` - check `status` before trusting `out`.
4. Web: `webread(url)` for GET, `webwrite(url, data)` for POST, `websave(file, url)` to download.
5. Validate and convert types at the boundary; external calls return raw bytes/strings that need parsing.

## Gotchas
- A `dlcall` prototype mismatch crashes or corrupts memory - get the argument/return types exactly right.
- Never put credentials or personal data in a URL; a wrong host leaks them.
- `system` output and exit status are separate; a nonzero status means the command failed even if `out` is non-empty.

## Done when
The external library/interpreter/command/service is called successfully, the status/return is checked, and the result is converted into a usable Nelson value.
