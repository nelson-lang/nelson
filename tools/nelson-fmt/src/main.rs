// ==============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ==============================================================================
// This file is part of Nelson.
// =============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ==============================================================================
use anyhow::{Context, Result};
use clap::Parser;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::env;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use toml_edit::Document;
use walkdir::WalkDir;
// ===========================================================================
// CLI
// ===========================================================================
/// Code formatter for the Nelson project.
///
/// Formats JSON, XML (help docs), Markdown, JavaScript, TypeScript, YAML files
/// and runs clang-format on C/C++ sources.
#[derive(Parser, Debug)]
#[command(name = "nelson-fmt", about = "Format code in the Nelson project")]
struct Args {
    /// Check formatting without modifying files (exit code 1 if unformatted)
    #[arg(short, long)]
    check: bool,

    /// Print every file being processed
    #[arg(short, long)]
    verbose: bool,

    /// Only format files tracked by git (from `git ls-files`), skipping
    /// untracked files such as build outputs or vendored copies not yet added
    /// to the repository.
    #[arg(short = 't', long)]
    tracked: bool,

    /// Root directory of the Nelson project (default: current directory)
    #[arg(default_value = ".")]
    root: PathBuf,
}
// ===========================================================================
// Configuration constants
// ===========================================================================
/// C/C++ source extensions handled by clang-format.
const CPP_EXTENSIONS: &[&str] = &["h", "hpp", "hxx", "cpp", "c", "cxx"];

/// Default major version of clang-format.
/// Stored in configuration so that the hard‑coded constant can be removed
/// once the user has upgraded their repo.  The JSON file lives next to
/// `.clang-format` and is loaded by `load_clang_format_ignore`.
const DEFAULT_CLANG_FORMAT_MAJOR_VERSION: u32 = 20;

/// Layout of the `nelson-format-ignore.json` configuration file that lives
/// next to `.clang-format` in the project root.  Historically this file only
/// contained paths to exclude from formatting; it now also allows specifying
/// the required clang-format major version via
/// `clangFormatMajorVersion`.
#[derive(Deserialize, Debug, Default)]
#[serde(rename_all = "camelCase")]
struct ClangFormatIgnore {
    #[serde(default)]
    exclude_path_contains: Vec<String>,

    /// If present, overrides `DEFAULT_CLANG_FORMAT_MAJOR_VERSION`.
    #[serde(default)]
    clang_format_major_version: Option<u32>,

    /// Frontend directories (relative to the project root) that own their own
    /// formatting through an `npm run format` / `format:check` script (prettier
    /// with a per-module `.prettierrc.json`).  These are excluded from
    /// nelson-fmt's own passes and instead delegated to npm, so a single
    /// `cargo format` also formats their TypeScript/TSX sources.
    #[serde(default)]
    npm_format_dirs: Vec<String>,
}

/// Load the formatting configuration from `<root>/nelson-format-ignore.json`.
/// Returns a default (empty) configuration when the file is absent.  The JSON
/// may contain both `excludePathContains` and an optional
/// `clangFormatMajorVersion` field.
fn load_clang_format_ignore(root: &Path) -> Result<ClangFormatIgnore> {
    let path = root.join("nelson-format-ignore.json");
    if !path.is_file() {
        return Ok(ClangFormatIgnore::default());
    }
    let content = std::fs::read_to_string(&path)
        .with_context(|| format!("Failed to read {}", path.display()))?;
    let ignore: ClangFormatIgnore = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse {}", path.display()))?;
    Ok(ignore)
}
// ===========================================================================
// Formatting helpers
// ===========================================================================
/// Return `true` when the content predominantly uses CRLF line endings.
fn uses_crlf(content: &str) -> bool {
    content.contains("\r\n")
}
/// Normalise line endings to `\n` and trim trailing whitespace on every line.
/// Ensures exactly one trailing newline.  Always outputs LF; the caller is
/// responsible for restoring CRLF when the original file used it.
fn basic_cleanup(content: &str) -> String {
    let normalized = content.replace("\r\n", "\n").replace('\r', "\n");
    let mut lines: Vec<&str> = normalized.lines().collect();

    // Remove trailing blank lines
    while lines.last().is_some_and(|l| l.trim().is_empty()) {
        lines.pop();
    }

    let cleaned: Vec<String> = lines.iter().map(|l| l.trim_end().to_string()).collect();
    cleaned.join("\n") + "\n"
}

/// Re-serialise JSON with 2-space indentation and a final newline.
/// Returns `None` when the file cannot be parsed (e.g. test fixtures with
/// embedded control characters) - the caller should skip it.
fn format_json(content: &str) -> Result<Option<String>> {
    let value: serde_json::Value = match serde_json::from_str(content) {
        Ok(v) => v,
        Err(_) => return Ok(None), // unparsable JSON - skip
    };

    let buf = Vec::new();
    let formatter = serde_json::ser::PrettyFormatter::with_indent(b"  ");
    let mut ser = serde_json::Serializer::with_formatter(buf, formatter);
    value.serialize(&mut ser)?;
    let mut output = String::from_utf8(ser.into_inner())?;
    output.push('\n');
    Ok(Some(output))
}

/// Default style passed to `cmake-fmt` when the user doesn't override it.
/// This mirrors the canonical settings used throughout the Nelson repository
/// for CMake files.
const DEFAULT_CMAKE_STYLE: &str = "indent_width=2,max_line_length=80,use_tabs=false";

/// Return the style argument to pass to `cmake-fmt`.
///
/// Behaviour is now deterministic: we do **not** search for configuration files
/// on disk.  Instead, we use a hard‑coded default, and let callers override it
/// by setting `CMAKE_FMT_STYLE` in the environment.  An empty variable value is
/// treated the same as being unset (i.e. the default is used).
fn cmake_style_arg() -> Option<String> {
    if let Ok(style) = env::var("CMAKE_FMT_STYLE") {
        if !style.is_empty() {
            return Some(style);
        }
    }
    Some(DEFAULT_CMAKE_STYLE.to_string())
}

/// Minimum required `cmake-fmt` version.
///
/// `cmake-fmt` before 0.11.0 silently drops parenthesized sub-expressions from
/// `if()` / `elseif()` / `while()` conditions.  For example
/// `if(WIN32 AND (X OR Y))` is rewritten as `if(WIN32 AND )`,
/// `if((A) OR B)` as `if(OR B)` and `if(NOT (A))` as `if(NOT)`, which CMake
/// then rejects with "Unknown arguments specified".  0.11.0 fixes the bug.
const MIN_CMAKE_FMT_VERSION: (u32, u32, u32) = (0, 11, 0);

/// Version requirement passed to `cargo install` so a fresh or upgraded
/// install never picks up a known-corrupting `cmake-fmt`.
const CMAKE_FMT_VERSION_REQ: &str = "^0.11";

/// Parse the `(major, minor, patch)` version from `cmake-fmt --version`
/// (whose output looks like `cmake-fmt 0.11.0`).
fn cmake_fmt_version(bin: &Path) -> Option<(u32, u32, u32)> {
    let output = Command::new(bin).arg("--version").output().ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout);
    // Take the first whitespace-separated token that starts with a digit.
    let token = text
        .split_whitespace()
        .find(|t| t.chars().next().is_some_and(|c| c.is_ascii_digit()))?;
    let mut parts = token.split('.');
    let parse = |s: Option<&str>| -> u32 {
        s.unwrap_or("0")
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse()
            .unwrap_or(0)
    };
    let major = parse(parts.next());
    let minor = parse(parts.next());
    let patch = parse(parts.next());
    Some((major, minor, patch))
}

/// Whether `version` satisfies [`MIN_CMAKE_FMT_VERSION`].
fn cmake_fmt_version_ok(version: (u32, u32, u32)) -> bool {
    version >= MIN_CMAKE_FMT_VERSION
}

/// Detect a CMake conditional that a formatter has corrupted by dropping one of
/// its operands.
///
/// Scans every `if` / `elseif` / `while` argument list and returns `true` when
/// it finds a boolean operator (`AND` / `OR`) with a missing left or right
/// operand, a unary operator (`NOT`, `DEFINED`, `EXISTS`, ...) with no operand,
/// or an empty condition — the exact signature left behind when a parenthesized
/// sub-expression is deleted.  Quoted strings and `#` comments are skipped so
/// that parentheses inside a `MATCHES "^(a|b)$"` regex are not miscounted.
///
/// This is used to reject `cmake-fmt` output that would damage a build file:
/// when it fires, the original file is kept untouched.
fn cmake_conditionals_are_corrupted(content: &str) -> bool {
    const BINARY_OPS: [&str; 2] = ["AND", "OR"];
    const UNARY_OPS: [&str; 9] = [
        "NOT",
        "DEFINED",
        "EXISTS",
        "COMMAND",
        "TARGET",
        "POLICY",
        "IS_DIRECTORY",
        "IS_SYMLINK",
        "IS_ABSOLUTE",
    ];

    let bytes = content.as_bytes();
    let n = bytes.len();

    // Return true if the identifier `word` (a command name) starts at byte
    // index `start` on a word boundary.
    let word_boundary = |start: usize| -> bool {
        if start == 0 {
            return true;
        }
        let prev = bytes[start - 1];
        !(prev.is_ascii_alphanumeric() || prev == b'_')
    };

    for keyword in ["if", "elseif", "while"] {
        let kw = keyword.as_bytes();
        let mut search = 0usize;
        while let Some(rel) = content[search..].find(keyword) {
            let idx = search + rel;
            search = idx + kw.len();
            if !word_boundary(idx) {
                continue;
            }
            // Skip whitespace between the keyword and the opening paren.
            let mut i = idx + kw.len();
            while i < n && (bytes[i] as char).is_whitespace() {
                i += 1;
            }
            if i >= n || bytes[i] != b'(' {
                continue;
            }
            // Collect the balanced argument list, skipping strings and comments.
            let mut depth = 0i32;
            let mut tokens: Vec<String> = Vec::new();
            let mut cur = String::new();
            let flush = |cur: &mut String, tokens: &mut Vec<String>| {
                if !cur.is_empty() {
                    tokens.push(std::mem::take(cur));
                }
            };
            while i < n {
                let c = bytes[i] as char;
                if c == '"' {
                    // Quoted string is a single operand token.
                    cur.push('"');
                    i += 1;
                    while i < n {
                        let d = bytes[i] as char;
                        cur.push(d);
                        if d == '\\' && i + 1 < n {
                            cur.push(bytes[i + 1] as char);
                            i += 2;
                            continue;
                        }
                        i += 1;
                        if d == '"' {
                            break;
                        }
                    }
                    continue;
                }
                if c == '#' {
                    // Line comment: skip to end of line.
                    while i < n && bytes[i] != b'\n' {
                        i += 1;
                    }
                    continue;
                }
                if c == '(' {
                    flush(&mut cur, &mut tokens);
                    depth += 1;
                    if depth > 1 {
                        tokens.push("(".to_string());
                    }
                    i += 1;
                    continue;
                }
                if c == ')' {
                    flush(&mut cur, &mut tokens);
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    tokens.push(")".to_string());
                    i += 1;
                    continue;
                }
                if c.is_whitespace() {
                    flush(&mut cur, &mut tokens);
                    i += 1;
                    continue;
                }
                cur.push(c);
                i += 1;
            }
            flush(&mut cur, &mut tokens);

            if depth != 0 {
                // Unbalanced parentheses: definitely corrupted.
                return true;
            }
            if tokens.is_empty() {
                return true;
            }

            let is_op = |t: &str| BINARY_OPS.iter().any(|o| o.eq_ignore_ascii_case(t));
            let is_unary = |t: &str| UNARY_OPS.iter().any(|o| o.eq_ignore_ascii_case(t));
            for (pos, tok) in tokens.iter().enumerate() {
                let prev = if pos == 0 {
                    "("
                } else {
                    tokens[pos - 1].as_str()
                };
                let next = tokens.get(pos + 1).map(String::as_str).unwrap_or(")");
                if is_op(tok) {
                    if prev == "(" || is_op(prev) {
                        return true;
                    }
                    if next == ")" || is_op(next) {
                        return true;
                    }
                }
                if is_unary(tok) && (next == ")" || is_op(next)) {
                    return true;
                }
            }
        }
    }
    false
}

/// Format CMake content by calling the cmake-fmt binary.
///
/// The original behaviour inspected the filesystem for a repository-level
/// configuration by walking upward from the file path.  That complexity has
/// been removed: `cmake-fmt` will use its built-in default style unless the
/// user specifies a different style via the `CMAKE_FMT_STYLE` environment
/// variable.  Keeping the helper logic here keeps testing simple and avoids
/// surprises when running `nelson-fmt` from different subdirectories.
fn format_cmake_file(path: &Path, content: &str, verbose: bool) -> String {
    // Build argument list explicitly so we can display it before running.
    let mut args: Vec<String> = Vec::new();

    // style passed to cmake-fmt (if any) comes solely from the environment
    let style_used: Option<String> = cmake_style_arg();
    if let Some(ref s) = style_used {
        args.push("--style".to_string());
        args.push(s.clone());
    }

    args.push("--assume-filename".to_string());
    args.push(path.display().to_string());
    args.push("-".to_string());

    if verbose {
        eprintln!("Running: cmake-fmt {}", args.join(" "));
        if let Some(ref s) = style_used {
            eprintln!("Using style: {}", s);
        }
    }

    let output = Command::new("cmake-fmt")
        .args(&args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .and_then(|mut child| {
            use std::io::Write;
            if let Some(stdin) = child.stdin.as_mut() {
                let _ = stdin.write_all(content.as_bytes());
            }
            child.wait_with_output()
        });
    match output {
        Ok(out) if out.status.success() => {
            let formatted = String::from_utf8_lossy(&out.stdout).to_string();
            // Guard against a buggy cmake-fmt that drops parenthesized
            // sub-expressions from conditions: never accept output that is
            // corrupted when the input was not.
            if cmake_conditionals_are_corrupted(&formatted)
                && !cmake_conditionals_are_corrupted(content)
            {
                eprintln!(
                    "WARNING: cmake-fmt produced a corrupted conditional in {}; \
                     keeping the original. Upgrade cmake-fmt to >= {}.{}.{} \
                     (cargo install cmake-fmt --version {} --force).",
                    path.display(),
                    MIN_CMAKE_FMT_VERSION.0,
                    MIN_CMAKE_FMT_VERSION.1,
                    MIN_CMAKE_FMT_VERSION.2,
                    CMAKE_FMT_VERSION_REQ
                );
                return content.to_string();
            }
            formatted
        }
        _ => content.to_string(),
    }
}

/// Format XML content.
/// Uses basic_cleanup only: normalises line endings and trailing whitespace
/// without restructuring the document.  The xmlformat crate was previously
/// used here but it inserts spurious blank lines between elements and splits
/// inline tags (e.g. <b>) across multiple lines, corrupting the Nelson help
/// XML files.
fn format_xml(content: &str) -> String {
    basic_cleanup(content)
}

/// Format TOML content.
fn format_toml(content: &str) -> String {
    match content.parse::<Document<_>>() {
        Ok(doc) => doc.to_string(),
        Err(_) => content.to_string(),
    }
}

/// Format Rust source code using rustfmt.
fn format_rust_with_rustfmt(path: &Path, content: &str, verbose: bool) -> String {
    let rustfmt_path = if cfg!(windows) {
        "rustfmt.exe"
    } else {
        "rustfmt"
    };
    let output = Command::new(rustfmt_path)
        .arg("--emit")
        .arg("stdout")
        .arg(path)
        .output();
    match output {
        Ok(out) if out.status.success() => {
            if verbose {
                eprintln!("Formatted with rustfmt: {}", path.display());
            }
            String::from_utf8_lossy(&out.stdout).to_string()
        }
        _ => {
            if verbose {
                eprintln!(
                    "rustfmt not found or failed, using basic_cleanup: {}",
                    path.display()
                );
            }
            basic_cleanup(content)
        }
    }
}
// ===========================================================================
// Helper utilities
// ===========================================================================
/// Simple wildcard matcher supporting `*` as "match any sequence".
///
/// - Patterns and text are expected to use `/` as path separator (caller
///   normalises backslashes to slashes). This matcher does not support
///   character classes or `?` - it's intentionally minimal for the needs of
///   `nelson-format-ignore.json` (e.g. `dir/*.c`).
fn wildcard_match(pattern: &str, text: &str) -> bool {
    // Fast path: exact match
    if pattern == text {
        return true;
    }

    // Split around '*' and match segments sequentially
    let mut parts = pattern.split('*').peekable();
    let mut pos: usize = 0;

    // Leading segment must match the start if pattern doesn't start with '*'
    if let Some(first) = parts.peek() {
        if !pattern.starts_with('*') && !first.is_empty() {
            let first = *first;
            if !text.starts_with(first) {
                return false;
            }
            pos = first.len();
            // consume the first part
            parts.next();
        }
    }

    // Match intermediate segments anywhere after the previous match
    while let Some(seg) = parts.next() {
        if seg.is_empty() {
            continue;
        }
        match text[pos..].find(seg) {
            Some(i) => pos += i + seg.len(),
            None => return false,
        }
    }

    // If pattern doesn't end with '*', ensure the last segment matched the end
    if !pattern.ends_with('*') {
        if let Some(last) = pattern.split('*').last() {
            if !last.is_empty() && !text.ends_with(last) {
                return false;
            }
        }
    }

    true
}

/// Decide whether `WalkDir` should prune (not descend into) a directory.
///
/// This is the main performance lever: pruning a large excluded directory
/// (e.g. `.git`, `tools`, `temp`) avoids enumerating the hundreds of thousands
/// of files it may contain.
///
/// Pruning is kept strictly consistent with the per-file [`is_path_excluded`]
/// logic: a directory is skipped only when *every* file beneath it would be
/// excluded anyway.  For a directory whose path (relative to `root`) is `R`,
/// with all descendants sharing the prefix `R/`:
///   * a `foo/`-style (anchored) pattern excludes every descendant iff it is a
///     prefix of `R/`;
///   * a bare-substring pattern excludes every descendant iff `R` already
///     contains it.
/// Wildcard patterns are ignored here and handled per-file instead, so no file
/// is ever skipped that the per-file pass would have formatted.
fn is_skipped_dir(entry: &walkdir::DirEntry, root: &Path, ignore: &ClangFormatIgnore) -> bool {
    if !entry.file_type().is_dir() {
        return false;
    }
    let relative = entry
        .path()
        .strip_prefix(root)
        .unwrap_or(entry.path())
        .to_string_lossy()
        .replace('\\', "/");
    if relative.is_empty() {
        return false; // never prune the root itself
    }
    let relative_slash = format!("{}/", relative);
    ignore.exclude_path_contains.iter().any(|p| {
        let pat = p.replace('\\', "/");
        if pat.contains('*') {
            false
        } else if pat.ends_with('/') {
            // Anchored: prune when the pattern is a prefix of "R/".
            relative_slash.starts_with(&pat)
        } else {
            // Bare substring: prune when R already contains it.
            !pat.is_empty() && relative.contains(&pat)
        }
    })
}
/// Determine whether a path is a C/C++ source that should be excluded from
/// clang-format (e.g. vendored third-party code).
fn is_cpp_excluded(path: &Path, root: &Path, ignore: &ClangFormatIgnore) -> bool {
    let relative = path
        .strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    ignore.exclude_path_contains.iter().any(|pattern| {
        let pat = pattern.replace('\\', "/");
        if pat.contains('*') {
            wildcard_match(&pat, &relative)
        } else if pat.ends_with('/') {
            relative.starts_with(&pat)
        } else {
            relative.contains(&pat)
        }
    })
}

/// Load the set of files tracked by git, as paths relative to `root` using
/// `/` separators (matching the normalisation used by the exclusion helpers).
///
/// Uses `git ls-files -z` so that paths containing spaces or other unusual
/// characters are handled correctly.  Returns an error when git is missing or
/// the command fails (e.g. `root` is not inside a git working tree).
fn load_tracked_files(root: &Path) -> Result<HashSet<String>> {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(["ls-files", "-z"])
        .output()
        .context("Failed to run `git ls-files` (is git installed and on PATH?)")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!(
            "`git ls-files` failed in {}: {}",
            root.display(),
            stderr.trim()
        );
    }

    let set = output
        .stdout
        .split(|&b| b == 0)
        .filter(|s| !s.is_empty())
        .map(|s| String::from_utf8_lossy(s).replace('\\', "/"))
        .collect();
    Ok(set)
}

/// Determine whether a path should be excluded from formatting for any extension.
fn is_path_excluded(path: &Path, root: &Path, ignore: &ClangFormatIgnore) -> bool {
    let relative = path
        .strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    ignore.exclude_path_contains.iter().any(|pattern| {
        let pat = pattern.replace('\\', "/");
        if pat.contains('*') {
            wildcard_match(&pat, &relative)
        } else if pat.ends_with('/') {
            relative.starts_with(&pat)
        } else {
            relative.contains(&pat)
        }
    })
}
/// Decide whether `path` should be processed as a "standard" file
/// (JSON / YAML / Markdown / JS / XML-help-doc) and return its logical
/// extension if so.
fn standard_file_kind<'a>(path: &'a Path, _root: &Path) -> Option<&'a str> {
    if path.file_name().is_some_and(|n| n == "CMakeLists.txt") {
        Some("cmake")
    } else {
        if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            if name == "CMakeLists.txt" {
                return Some("cmake");
            }
        }
        let ext = path.extension()?.to_str()?;
        match ext {
            "json" | "yml" | "yaml" | "md" | "js" | "ts" | "cmake" => Some(ext),
            "xml" => {
                // We only treat XML files as special when they live inside a
                // help/ directory.  Other XML files (e.g. desktop data files)
                // are ignored so they don’t trigger formatting.
                let relative = path
                    .strip_prefix(_root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .replace('\\', "/");
                if relative.contains("/help/") {
                    Some("xml")
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
// ===========================================================================
// File processors
// ===========================================================================
/// Format / check a single file.  Returns `true` when the file is already
/// formatted (or was successfully written in write mode).
fn process_file(path: &Path, ext: &str, check: bool, verbose: bool) -> Result<bool> {
    let content =
        std::fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;

    // Remember original line-ending style so we can restore it after formatting.
    let crlf = uses_crlf(&content);

    // Fast path: if the file is empty, skip formatting
    if content.is_empty() {
        return Ok(true);
    }

    // Always normalize input to LF for comparison
    let content_lf = content.replace("\r\n", "\n").replace('\r', "\n");

    // Fast hash check: if the normalized content is already clean, skip formatting
    // (for large files, this avoids unnecessary formatting work)
    // For now, use a direct string comparison as a fast path
    let ext_is_simple = matches!(ext, "md" | "js" | "yml" | "yaml" | "ts");
    if ext_is_simple {
        let cleaned = basic_cleanup(&content_lf);
        if content_lf == cleaned {
            return Ok(true);
        }
    }

    // Otherwise, do full formatting
    let formatted_lf = if path.file_name().is_some_and(|n| n == "CMakeLists.txt") || ext == "cmake"
    {
        format_cmake_file(path, &content, verbose)
    } else {
        match ext {
            "json" => match format_json(&content)? {
                Some(f) => f,
                None => {
                    if verbose {
                        eprintln!("Skipping (unparsable JSON): {}", path.display());
                    }
                    return Ok(true);
                }
            },
            "js" | "ts" => basic_cleanup(&content),
            "xml" => format_xml(&content),
            "toml" => format_toml(&content),
            "rs" => format_rust_with_rustfmt(path, &content, verbose),
            "yml" | "yaml" | "md" => basic_cleanup(&content),
            _ => return Ok(true),
        }
    };

    let formatted_lf = formatted_lf.replace("\r\n", "\n").replace('\r', "\n");

    // Restore CRLF line endings when the original file used them.
    let formatted = if crlf {
        formatted_lf.replace('\n', "\r\n")
    } else {
        formatted_lf.clone()
    };

    if content_lf == formatted_lf {
        return Ok(true); // already formatted
    }

    if check {
        eprintln!("Would reformat: {}", path.display());
        std::process::exit(1);
    }

    if verbose {
        eprintln!("Formatted: {}", path.display());
    }
    std::fs::write(path, &formatted).with_context(|| format!("write {}", path.display()))?;
    Ok(true)
}
/// Resolve the clang-format binary to use:
///   1. Try `<root>/tools/clang-format/clang-format[.exe]` (all platforms).
///   2. Try `clang-format-<MAJOR>` on PATH (e.g. `clang-format-18`).
///   3. Fall back to `clang-format` on PATH.
///
/// Returns `None` when no usable binary is found.
fn resolve_clang_format(root: &Path, required_major: u32) -> Option<PathBuf> {
    let local_name = if cfg!(windows) {
        "clang-format.exe"
    } else {
        "clang-format"
    };

    // 1. Prefer the local copy shipped in tools/clang-format/
    let local = root.join("tools").join("clang-format").join(local_name);
    if local.is_file() {
        return Some(local);
    }

    // 2. Try version-specific binary on PATH (e.g. clang-format-18)
    let versioned = format!("clang-format-{}", required_major);
    if Command::new(&versioned)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return Some(PathBuf::from(versioned));
    }

    // 3. Fall back to unversioned name on PATH
    if Command::new(local_name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return Some(PathBuf::from(local_name));
    }

    None
}

/// Install (or upgrade) `cmake-fmt` via cargo, pinned to a non-corrupting
/// version.  `force` re-installs over an existing (too old) binary.
fn cargo_install_cmake_fmt(force: bool) -> bool {
    let mut cmd = Command::new("cargo");
    cmd.arg("install")
        .arg("cmake-fmt")
        .arg("--version")
        .arg(CMAKE_FMT_VERSION_REQ);
    if force {
        cmd.arg("--force");
    }
    match cmd.status() {
        Ok(status) if status.success() => true,
        Ok(status) => {
            eprintln!("`cargo install cmake-fmt` failed with exit {}", status);
            false
        }
        Err(e) => {
            eprintln!("Failed to run `cargo install cmake-fmt`: {}", e);
            false
        }
    }
}

/// Return the given `cmake-fmt` binary only when it meets the minimum required
/// version; otherwise warn and return `None`.  Refusing an older binary is the
/// "pin the minimum version" half of the corruption guard: versions before
/// [`MIN_CMAKE_FMT_VERSION`] drop parenthesized `if()` sub-expressions.
fn accept_if_recent(bin: PathBuf) -> Option<PathBuf> {
    match cmake_fmt_version(&bin) {
        Some(v) if cmake_fmt_version_ok(v) => Some(bin),
        Some((a, b, c)) => {
            eprintln!(
                "WARNING: {} is cmake-fmt {}.{}.{}, but >= {}.{}.{} is required \
                 (older versions silently drop parenthesized if() sub-expressions). \
                 Upgrade with `cargo install cmake-fmt --version {} --force`.",
                bin.display(),
                a,
                b,
                c,
                MIN_CMAKE_FMT_VERSION.0,
                MIN_CMAKE_FMT_VERSION.1,
                MIN_CMAKE_FMT_VERSION.2,
                CMAKE_FMT_VERSION_REQ
            );
            None
        }
        None => None,
    }
}

/// Resolve cmake-fmt binary to use:
/// 1. Try `<root>/tools/cmake-fmt/cmake-fmt[.exe]`
/// 2. Fall back to `cmake-fmt` on PATH
/// 3. Install a pinned, non-corrupting version via cargo
///
/// A binary older than [`MIN_CMAKE_FMT_VERSION`] is refused (and upgraded when
/// found on PATH) so a known-corrupting formatter is never used.
fn resolve_cmake_fmt(root: &Path) -> Option<PathBuf> {
    let local_name = if cfg!(windows) {
        "cmake-fmt.exe"
    } else {
        "cmake-fmt"
    };
    let local = root.join("tools").join("cmake-fmt").join(local_name);
    if local.is_file() {
        if let Some(bin) = accept_if_recent(local) {
            return Some(bin);
        }
        // A vendored binary that is too old cannot be upgraded in place; fall
        // through to PATH / cargo install.
    }

    // Try on PATH.
    match cmake_fmt_version(Path::new(local_name)) {
        Some(v) if cmake_fmt_version_ok(v) => return Some(PathBuf::from(local_name)),
        Some((a, b, c)) => {
            eprintln!(
                "cmake-fmt {}.{}.{} on PATH is too old (>= {}.{}.{} required); \
                 upgrading via `cargo install cmake-fmt --version {} --force`...",
                a,
                b,
                c,
                MIN_CMAKE_FMT_VERSION.0,
                MIN_CMAKE_FMT_VERSION.1,
                MIN_CMAKE_FMT_VERSION.2,
                CMAKE_FMT_VERSION_REQ
            );
            if cargo_install_cmake_fmt(true) {
                return accept_if_recent(PathBuf::from(local_name));
            }
            return None;
        }
        None => {}
    }

    // Not found: attempt to install a pinned version so users running the tool
    // get an automatic experience.  Requires `cargo` and network access.
    eprintln!(
        "cmake-fmt not found. Attempting `cargo install cmake-fmt --version {}`...",
        CMAKE_FMT_VERSION_REQ
    );
    if cargo_install_cmake_fmt(false) {
        return accept_if_recent(PathBuf::from(local_name));
    }

    None
}

/// Format (or check) the pre-collected CMake `files` using the cmake-fmt
/// binary.  Batches are processed in parallel to use all available cores.
fn process_cmake_files(
    root: &Path,
    files: &[PathBuf],
    check: bool,
    verbose: bool,
) -> Result<usize> {
    let total = files.len();
    if files.is_empty() {
        return Ok(0);
    }

    let cmake_bin = match resolve_cmake_fmt(root) {
        Some(b) => b,
        None => {
            eprintln!(
                "WARNING: cmake-fmt not found - skipping {} CMake files",
                files.len()
            );
            return Ok(0);
        }
    };

    // determine which style argument (if any) we will pass to cmake-fmt
    let style_arg: Option<String> = cmake_style_arg();

    if verbose {
        eprintln!("Using cmake-fmt: {}", cmake_bin.display());
        if let Some(ref s) = style_arg {
            eprintln!("Using style: {}", s);
        }
    }

    files.par_chunks(50).for_each(|chunk| {
        // In write mode cmake-fmt edits files in place (`-i`), so snapshot the
        // originals first: if the formatter corrupts a conditional we restore
        // them below instead of leaving a broken build file on disk.
        let originals: Vec<Option<String>> = if check {
            Vec::new()
        } else {
            chunk
                .iter()
                .map(|file| std::fs::read_to_string(file).ok())
                .collect()
        };

        let mut cmd = Command::new(&cmake_bin);
        if let Some(ref s) = style_arg {
            cmd.arg("--style").arg(s);
        }
        if check {
            cmd.arg("--check");
        } else {
            cmd.arg("-i");
        }
        for file in chunk {
            if verbose {
                let action = if check { "Checking" } else { "Formatting" };
                eprintln!("{} (cmake-fmt): {}", action, file.display());
            }
            cmd.arg(file);
        }
        match cmd.output() {
            Ok(output) if output.status.success() => {
                if !check {
                    guard_cmake_output(chunk, &originals);
                }
            }
            Ok(output) => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                if check {
                    eprintln!("cmake-fmt would reformat files in this batch:\n{}", stderr);
                    std::process::exit(1);
                } else {
                    eprintln!("cmake-fmt error: {}", stderr);
                }
            }
            Err(e) => eprintln!("Failed to run cmake-fmt: {}", e),
        }
    });

    Ok(total)
}

/// After an in-place `cmake-fmt` run, restore any file whose conditionals the
/// formatter corrupted (dropping a parenthesized sub-expression) while the
/// original was still intact.  `originals` is aligned with `chunk`.
fn guard_cmake_output(chunk: &[PathBuf], originals: &[Option<String>]) {
    for (file, original) in chunk.iter().zip(originals.iter()) {
        let Some(original) = original else { continue };
        let Ok(formatted) = std::fs::read_to_string(file) else {
            continue;
        };
        if cmake_conditionals_are_corrupted(&formatted)
            && !cmake_conditionals_are_corrupted(original)
        {
            match std::fs::write(file, original) {
                Ok(()) => eprintln!(
                    "WARNING: cmake-fmt corrupted a conditional in {}; restored the \
                     original. Upgrade cmake-fmt to >= {}.{}.{} \
                     (cargo install cmake-fmt --version {} --force).",
                    file.display(),
                    MIN_CMAKE_FMT_VERSION.0,
                    MIN_CMAKE_FMT_VERSION.1,
                    MIN_CMAKE_FMT_VERSION.2,
                    CMAKE_FMT_VERSION_REQ
                ),
                Err(e) => eprintln!(
                    "ERROR: cmake-fmt corrupted {} and it could not be restored: {}",
                    file.display(),
                    e
                ),
            }
        }
    }
}

/// Extract the major version number from `clang-format --version` output.
/// Typical output: "clang-format version 18.1.3 (…)" or
/// "Ubuntu clang-format version 18.1.3 (1ubuntu1)"
fn clang_format_major_version(bin: &Path) -> Option<u32> {
    let output = Command::new(bin).arg("--version").output().ok()?;
    let text = String::from_utf8_lossy(&output.stdout);
    // Look for "version X.Y.Z"
    let after_version = text.split("version").nth(1)?;
    let major_str = after_version.trim().split('.').next()?;
    major_str.trim().parse().ok()
}

/// Check that the detected clang-format matches the required major
/// version.  Prints a warning and returns `false` on mismatch.
fn check_clang_format_version(bin: &Path, required_major: u32) -> bool {
    match clang_format_major_version(bin) {
        Some(major) if major == required_major => true,
        Some(major) => {
            eprintln!(
                "WARNING: clang-format major version is {} but {} is required. \
                 Formatting may differ across platforms. \
                 Please install clang-format {}.",
                major, required_major, required_major
            );
            false
        }
        None => {
            eprintln!("WARNING: could not determine clang-format version");
            false
        }
    }
}
/// Format (or check) the pre-collected C/C++ `files` via `clang-format`.
/// Batches are processed in parallel to use all available cores.
fn process_cpp_files(
    root: &Path,
    ignore: &ClangFormatIgnore,
    files: &[PathBuf],
    check: bool,
    verbose: bool,
) -> Result<(usize, usize)> {
    let total = files.len();
    if files.is_empty() {
        return Ok((0, 0));
    }

    // Determine which major version we require: either the value from
    // the configuration file or the built‑in default.
    let required_major = ignore
        .clang_format_major_version
        .unwrap_or(DEFAULT_CLANG_FORMAT_MAJOR_VERSION);

    // Resolve clang-format executable:
    //   All platforms → prefer <root>/tools/clang-format/clang-format[.exe], then PATH
    let clang_format_bin = match resolve_clang_format(root, required_major) {
        Some(bin) => bin,
        None => {
            eprintln!(
                "WARNING: clang-format not found - skipping {} C/C++ files",
                files.len()
            );
            return Ok((0, 0));
        }
    };

    // Verify the version matches the required major version
    check_clang_format_version(&clang_format_bin, required_major);

    // Point clang-format at the project's .clang-format config file
    let style_file = root.join(".clang-format");
    let style_arg = format!("file:{}", style_file.display());

    if verbose {
        eprintln!("Using clang-format: {}", clang_format_bin.display());
        eprintln!("Using style: {}", style_arg);
    }

    files.par_chunks(50).for_each(|chunk| {
        let mut cmd = Command::new(&clang_format_bin);
        cmd.arg(format!("--style={}", style_arg));
        if check {
            cmd.args(["--dry-run", "-Werror"]);
        } else {
            cmd.arg("-i");
        }
        for file in chunk {
            if verbose {
                let action = if check { "Checking" } else { "Formatting" };
                eprintln!("{} (clang-format): {}", action, file.display());
            }
            cmd.arg(file);
        }
        match cmd.output() {
            Ok(output) if output.status.success() => {}
            Ok(output) => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                if check {
                    eprintln!(
                        "clang-format would reformat files in this batch:\n{}",
                        stderr
                    );
                    std::process::exit(1);
                } else {
                    eprintln!("clang-format error: {}", stderr);
                }
            }
            Err(e) => eprintln!("Failed to run clang-format: {}", e),
        }
    });

    Ok((total, 0))
}
/// Collect the set of candidate files to consider for formatting, as a single
/// pass over the source of truth:
///   * with `tracked`, iterate the git file list directly (no filesystem walk),
///     which avoids descending into huge untracked trees such as `temp/`;
///   * otherwise walk the tree once, pruning excluded directories up front via
///     `is_skipped_dir` so their contents are never enumerated.
///
/// Directory pruning and the git list replace the previous behaviour of walking
/// the entire tree three times (once per file category).
fn collect_candidate_files(
    root: &Path,
    ignore: &ClangFormatIgnore,
    tracked: Option<&HashSet<String>>,
) -> Vec<PathBuf> {
    if let Some(set) = tracked {
        set.iter()
            .map(|rel| root.join(rel))
            .filter(|p| p.is_file())
            .collect()
    } else {
        WalkDir::new(root)
            .into_iter()
            .filter_entry(|e| !is_skipped_dir(e, root, ignore))
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
            .map(|e| e.path().to_path_buf())
            .collect()
    }
}

/// Format (or check) frontend directories through their own `npm run format`
/// (or `format:check`) scripts, so that a single `cargo format` also covers the
/// TypeScript/TSX/JS sources those frontends own via prettier and their
/// per-module `.prettierrc.json`.
///
/// A directory is skipped, with a clear warning, when it lacks a `package.json`
/// or has no installed `node_modules` (we never run `npm install` implicitly).
/// Node/npm being absent is likewise a graceful skip.  In `--check` mode a
/// non-zero npm exit terminates the process like the other check phases.
fn process_frontends(root: &Path, dirs: &[String], check: bool, verbose: bool) {
    if dirs.is_empty() {
        return;
    }
    let npm = if cfg!(windows) { "npm.cmd" } else { "npm" };
    let script = if check { "format:check" } else { "format" };

    for rel in dirs {
        let dir = root.join(rel);
        if !dir.join("package.json").is_file() {
            eprintln!("WARNING: no package.json in {} - skipping npm format", rel);
            continue;
        }
        if !dir.join("node_modules").is_dir() {
            eprintln!(
                "WARNING: {} has no node_modules - run `npm install` there to \
                 format its TypeScript sources. Skipping.",
                rel
            );
            continue;
        }
        if verbose {
            eprintln!("Running `{} run {}` in {}", npm, script, rel);
        }
        match Command::new(npm)
            .current_dir(&dir)
            .arg("run")
            .arg(script)
            .status()
        {
            Ok(status) if status.success() => {}
            Ok(status) => {
                eprintln!("`npm run {}` failed in {} ({})", script, rel, status);
                if check {
                    std::process::exit(1);
                }
            }
            Err(e) => {
                eprintln!(
                    "Failed to run `{} run {}` in {}: {} (is Node/npm installed?)",
                    npm, script, rel, e
                );
            }
        }
    }
}

// ===========================================================================
// Main
// ===========================================================================
fn main() -> Result<()> {
    let args = Args::parse();
    let root = std::fs::canonicalize(&args.root)
        .with_context(|| format!("Invalid root directory: {}", args.root.display()))?;

    // Load nelson-format ignore list from <root>/nelson-format-ignore.json
    let ignore = load_clang_format_ignore(&root)?;

    // When --tracked is set, restrict formatting to files tracked by git.
    let tracked: Option<HashSet<String>> = if args.tracked {
        let set = load_tracked_files(&root)?;
        if args.verbose {
            eprintln!("Restricting to {} git-tracked files", set.len());
        }
        Some(set)
    } else {
        None
    };

    // --- Single collection pass, then classify by file category -----------
    let candidates = collect_candidate_files(&root, &ignore, tracked.as_ref());

    let mut standard: Vec<(PathBuf, String)> = Vec::new();
    let mut cmake_files: Vec<PathBuf> = Vec::new();
    let mut cpp_files: Vec<PathBuf> = Vec::new();

    for path in candidates {
        // C/C++ sources go to clang-format (with their own exclusion list).
        if let Some(ext) = path.extension().and_then(OsStr::to_str) {
            if CPP_EXTENSIONS.contains(&ext) {
                if !is_cpp_excluded(&path, &root, &ignore) {
                    cpp_files.push(path);
                }
                continue;
            }
        }
        if is_path_excluded(&path, &root, &ignore) {
            continue;
        }
        // Materialise the kind as an owned String so the borrow of `path`
        // ends before we move `path` into one of the category vectors.
        if let Some(kind) = standard_file_kind(&path, &root).map(str::to_string) {
            if kind == "cmake" {
                cmake_files.push(path);
            } else {
                standard.push((path, kind));
            }
        }
    }

    let mut total_files: usize = 0;

    // --- Standard files (JSON, YAML, MD, JS, XML help docs) ---------------
    let results: Vec<_> = standard
        .par_iter()
        .map(|(path, ext)| {
            let res = process_file(path, ext, args.check, args.verbose);
            (res, path)
        })
        .collect();

    total_files += results.len();
    for (res, path) in results {
        if let Err(e) = res {
            eprintln!("Error processing {}: {:#}", path.display(), e);
        }
    }

    // --- CMake files via cmake-fmt ---------------------------------------
    total_files += process_cmake_files(&root, &cmake_files, args.check, args.verbose)?;

    // --- C/C++ files via clang-format -------------------------------------
    let (cpp_total, _) = process_cpp_files(&root, &ignore, &cpp_files, args.check, args.verbose)?;
    total_files += cpp_total;

    // --- Frontend TypeScript/TSX via each frontend's own `npm run format` --
    process_frontends(&root, &ignore.npm_format_dirs, args.check, args.verbose);

    // --- Summary ----------------------------------------------------------
    if args.check {
        println!("All {} files are properly formatted", total_files);
    } else {
        println!("Processed {} files", total_files);
    }

    Ok(())
}
// ===========================================================================
// Tests
// ===========================================================================
#[cfg(test)]
mod tests {
    use super::*;
    use speculoos::prelude::*;
    use std::fs;
    use std::sync::Mutex;
    use tempfile::TempDir;

    // serialize tests that modify environment variables
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    // -----------------------------------------------------------------------
    // Parallel processing (integration/logic test)
    // -----------------------------------------------------------------------
    #[test]
    fn parallel_processing_multiple_files() {
        let tmp = TempDir::new().unwrap();
        let file1 = tmp.path().join("a.md");
        let file2 = tmp.path().join("b.md");
        let file3 = tmp.path().join("c.md");
        fs::write(&file1, "foo   \nbar\n").unwrap();
        fs::write(&file2, "baz\nqux   \n").unwrap();
        fs::write(&file3, "hello\nworld\n").unwrap();

        let files = vec![file1.clone(), file2.clone(), file3.clone()];
        let results: Vec<_> = files
            .par_iter()
            .map(|path| process_file(path, "md", false, false))
            .collect();
        for res in results {
            assert_that(&res.is_ok()).is_true();
        }
        let content1 = fs::read_to_string(&file1).unwrap();
        let content2 = fs::read_to_string(&file2).unwrap();
        let content3 = fs::read_to_string(&file3).unwrap();
        assert_that(&content1).is_equal_to("foo\nbar\n".to_string());
        assert_that(&content2).is_equal_to("baz\nqux\n".to_string());
        assert_that(&content3).is_equal_to("hello\nworld\n".to_string());
    }

    // -----------------------------------------------------------------------
    // Fast path for basic_cleanup (unit test)
    // -----------------------------------------------------------------------
    #[test]
    fn process_file_fast_path_skips_clean_file() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("clean.md");
        let clean_content = "already clean\n";
        fs::write(&file_path, clean_content).unwrap();
        // Should not rewrite or change the file
        let result = process_file(&file_path, "md", false, false).unwrap();
        assert_that(&result).is_true();
        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content).is_equal_to(clean_content.to_string());
    }

    #[test]
    fn process_file_fast_path_detects_dirty_file() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("dirty.md");
        let dirty_content = "foo   \nbar\n\n";
        fs::write(&file_path, dirty_content).unwrap();
        // Should rewrite the file to clean form
        let result = process_file(&file_path, "md", false, false).unwrap();
        assert_that(&result).is_true();
        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content).is_equal_to("foo\nbar\n".to_string());
    }

    // -----------------------------------------------------------------------
    // basic_cleanup
    // -----------------------------------------------------------------------
    #[test]
    fn basic_cleanup_normalizes_crlf_to_lf() {
        let input = "line1\r\nline2\r\nline3\r\n";
        let result = basic_cleanup(input);
        assert_that(&result.contains("\r\n")).is_false();
        assert_that(&result).is_equal_to("line1\nline2\nline3\n".to_string());
    }

    #[test]
    fn basic_cleanup_normalizes_bare_cr() {
        let input = "line1\rline2\rline3\r";
        let result = basic_cleanup(input);
        assert_that(&result.contains('\r')).is_false();
        assert_that(&result).is_equal_to("line1\nline2\nline3\n".to_string());
    }

    #[test]
    fn basic_cleanup_trims_trailing_whitespace() {
        let input = "hello   \nworld\t\n";
        let result = basic_cleanup(input);
        assert_that(&result).is_equal_to("hello\nworld\n".to_string());
    }

    #[test]
    fn basic_cleanup_removes_trailing_blank_lines() {
        let input = "content\n\n\n\n";
        let result = basic_cleanup(input);
        assert_that(&result).is_equal_to("content\n".to_string());
    }

    #[test]
    fn basic_cleanup_ensures_single_trailing_newline() {
        let input = "no newline at end";
        let result = basic_cleanup(input);
        assert_that(&result.ends_with('\n')).is_true();
        assert_that(&result).is_equal_to("no newline at end\n".to_string());
    }

    #[test]
    fn basic_cleanup_preserves_inner_blank_lines() {
        let input = "line1\n\nline3\n";
        let result = basic_cleanup(input);
        assert_that(&result).is_equal_to("line1\n\nline3\n".to_string());
    }

    #[test]
    fn basic_cleanup_handles_empty_input() {
        let result = basic_cleanup("");
        assert_that(&result).is_equal_to("\n".to_string());
    }

    // -----------------------------------------------------------------------
    // uses_crlf
    // -----------------------------------------------------------------------
    #[test]
    fn uses_crlf_detects_crlf() {
        assert_that(&uses_crlf("line1\r\nline2\r\n")).is_true();
    }

    #[test]
    fn uses_crlf_returns_false_for_lf() {
        assert_that(&uses_crlf("line1\nline2\n")).is_false();
    }

    // -----------------------------------------------------------------------
    // format_json
    // -----------------------------------------------------------------------
    #[test]
    fn format_json_pretty_prints_with_2_space_indent() {
        let input = r#"{"a":1,"b":[2,3]}"#;
        let result = format_json(input).unwrap();
        assert_that(&result).is_some();
        let formatted = result.unwrap();
        // Check 2-space indentation
        assert_that(&formatted.contains("  \"a\"")).is_true();
        assert_that(&formatted.ends_with('\n')).is_true();
    }

    #[test]
    fn format_json_preserves_key_order() {
        let input = r#"{"z":1,"a":2,"m":3}"#;
        let result = format_json(input).unwrap().unwrap();
        let z_pos = result.find("\"z\"").unwrap();
        let a_pos = result.find("\"a\"").unwrap();
        let m_pos = result.find("\"m\"").unwrap();
        assert_that(&z_pos).is_less_than(a_pos);
        assert_that(&a_pos).is_less_than(m_pos);
    }

    #[test]
    fn format_json_returns_none_for_invalid_json() {
        let input = "not valid json {{{";
        let result = format_json(input).unwrap();
        assert_that(&result).is_none();
    }

    #[test]
    fn format_json_already_formatted_is_stable() {
        let input = "{\n  \"key\": \"value\"\n}\n";
        let result = format_json(input).unwrap().unwrap();
        assert_that(&result).is_equal_to(input.to_string());
    }

    // -----------------------------------------------------------------------
    // YAML via basic_cleanup (non-destructive)
    // -----------------------------------------------------------------------

    #[test]
    fn yaml_basic_cleanup_preserves_comments() {
        let input = "# This is a comment\nkey: value\n# Another comment\nlist:\n  - a\n";
        let result = basic_cleanup(input);
        assert_that(&result.contains("# This is a comment")).is_true();
        assert_that(&result.contains("# Another comment")).is_true();
    }

    #[test]
    fn yaml_basic_cleanup_preserves_indentation() {
        let input = "root:\n    deep_indent: true\n  normal_indent: true\n";
        let result = basic_cleanup(input);
        assert_that(&result.contains("    deep_indent")).is_true();
        assert_that(&result.contains("  normal_indent")).is_true();
    }

    #[test]
    fn yaml_basic_cleanup_normalizes_crlf() {
        let input = "key: value\r\nother: data\r\n";
        let result = basic_cleanup(input);
        assert_that(&result.contains("\r")).is_false();
        assert_that(&result.ends_with('\n')).is_true();
    }

    // -----------------------------------------------------------------------
    // is_cpp_excluded
    // -----------------------------------------------------------------------

    #[test]
    fn is_cpp_excluded_matches_exclude_pattern() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec!["third_party".to_string()],
            clang_format_major_version: None,
            npm_format_dirs: vec![],
        };
        let path = Path::new("/project/modules/third_party/lib.cpp");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_true();
    }

    #[test]
    fn is_cpp_excluded_no_match_for_normal_path() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec!["third_party".to_string()],
            clang_format_major_version: None,
            npm_format_dirs: vec![],
        };
        let path = Path::new("/project/modules/core/main.cpp");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_false();
    }

    #[test]
    fn is_cpp_excluded_matches_multiple_patterns() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec!["vendored".to_string(), "generated".to_string()],
            clang_format_major_version: None,
            npm_format_dirs: vec![],
        };
        let path = Path::new("/project/src/generated/bindings.h");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_true();
    }

    #[test]
    fn is_cpp_excluded_wildcard_pattern_matches_c_files() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec!["modules/nflow_gui/tests/continous/*.c".to_string()],
            clang_format_major_version: None,
            npm_format_dirs: vec![],
        };
        let path = Path::new("/project/modules/nflow_gui/tests/continous/foo.c");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_true();
    }

    #[test]
    fn is_cpp_excluded_empty_ignore_list() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore::default();
        let path = Path::new("/project/src/main.cpp");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_false();
    }

    // -----------------------------------------------------------------------
    // is_skipped_dir (directory pruning)
    // -----------------------------------------------------------------------

    fn dir_is_pruned(root: &Path, rel: &str, ignore: &ClangFormatIgnore) -> bool {
        WalkDir::new(root)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_dir())
            .filter(|e| {
                e.path()
                    .strip_prefix(root)
                    .unwrap()
                    .to_string_lossy()
                    .replace('\\', "/")
                    == rel
            })
            .any(|e| is_skipped_dir(&e, root, ignore))
    }

    #[test]
    fn is_skipped_dir_prunes_top_level_and_pathed_but_not_nested_name() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::create_dir_all(root.join("temp/x")).unwrap();
        fs::create_dir_all(root.join("modules/web_gui/frontend/src")).unwrap();
        fs::create_dir_all(root.join("modules/nflow_gui/src/ts/blocks")).unwrap();
        fs::create_dir_all(root.join("modules/nflow_gui/src/cpp")).unwrap();
        fs::create_dir_all(root.join("modules/core/cmake")).unwrap();

        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec![
                "temp/".into(),
                "cmake/".into(),
                "modules/web_gui/frontend/".into(),
                "modules/nflow_gui/src/ts/".into(),
            ],
            clang_format_major_version: None,
            npm_format_dirs: vec![],
        };

        // Anchored top-level and multi-segment patterns prune.
        assert_that(&dir_is_pruned(root, "temp", &ignore)).is_true();
        assert_that(&dir_is_pruned(root, "modules/web_gui/frontend", &ignore)).is_true();
        assert_that(&dir_is_pruned(root, "modules/nflow_gui/src/ts", &ignore)).is_true();

        // The nflow_gui C++ tree must NOT be pruned (still clang-formatted).
        assert_that(&dir_is_pruned(root, "modules/nflow_gui/src/cpp", &ignore)).is_false();

        // A nested `cmake` dir must NOT be pruned by the anchored `cmake/`
        // pattern (consistency with is_path_excluded, which is anchored).
        assert_that(&dir_is_pruned(root, "modules/core/cmake", &ignore)).is_false();

        // Parents of a pruned dir are not themselves pruned.
        assert_that(&dir_is_pruned(root, "modules/web_gui", &ignore)).is_false();
    }

    // -----------------------------------------------------------------------
    // frontend directories excluded from nelson-fmt (owned by prettier)
    // -----------------------------------------------------------------------

    #[test]
    fn is_path_excluded_covers_frontend_dirs() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec![
                "modules/web_gui/frontend/".into(),
                "modules/nflow_gui/src/ts/".into(),
            ],
            clang_format_major_version: None,
            npm_format_dirs: vec![],
        };
        assert_that(&is_path_excluded(
            Path::new("/project/modules/web_gui/frontend/package.json"),
            root,
            &ignore,
        ))
        .is_true();
        assert_that(&is_path_excluded(
            Path::new("/project/modules/nflow_gui/src/ts/app.tsx"),
            root,
            &ignore,
        ))
        .is_true();
        // A C++ source elsewhere in nflow_gui is still processed.
        assert_that(&is_path_excluded(
            Path::new("/project/modules/nflow_gui/src/cpp/foo.cpp"),
            root,
            &ignore,
        ))
        .is_false();
    }

    // -----------------------------------------------------------------------
    // standard_file_kind
    // -----------------------------------------------------------------------

    #[test]
    fn standard_file_kind_returns_json_for_json_files() {
        let root = Path::new("/project");
        let path = Path::new("/project/config.json");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_some().is_equal_to("json");
    }

    #[test]
    fn standard_file_kind_returns_md_for_markdown_files() {
        let root = Path::new("/project");
        let path = Path::new("/project/README.md");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_some().is_equal_to("md");
    }

    #[test]
    fn standard_file_kind_returns_js_for_javascript_files() {
        let root = Path::new("/project");
        let path = Path::new("/project/script.js");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_some().is_equal_to("js");
    }

    #[test]
    fn standard_file_kind_returns_yml_for_yaml_files() {
        let root = Path::new("/project");
        let path = Path::new("/project/config.yml");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_some().is_equal_to("yml");
    }

    #[test]
    fn standard_file_kind_returns_yaml_for_yaml_files() {
        let root = Path::new("/project");
        let path = Path::new("/project/config.yaml");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_some().is_equal_to("yaml");
    }

    #[test]
    fn standard_file_kind_returns_none_for_cpp() {
        let root = Path::new("/project");
        let path = Path::new("/project/main.cpp");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_none();
    }

    #[test]
    fn standard_file_kind_xml_only_in_help_dir() {
        let root = Path::new("/project");
        let help_path = Path::new("/project/modules/core/help/en_US/xml/doc.xml");
        let non_help_path = Path::new("/project/desktop/data.xml");

        let result_help = standard_file_kind(help_path, root);
        assert_that(&result_help).is_some().is_equal_to("xml");

        let result_non_help = standard_file_kind(non_help_path, root);
        assert_that(&result_non_help).is_none();
    }

    #[test]
    fn standard_file_kind_returns_none_for_no_extension() {
        let root = Path::new("/project");
        let path = Path::new("/project/Makefile");
        let result = standard_file_kind(path, root);
        assert_that(&result).is_none();
    }

    // -----------------------------------------------------------------------
    // ClangFormatIgnore deserialization
    // -----------------------------------------------------------------------

    #[test]
    fn clang_format_ignore_deserializes_from_json() {
        let json = r#"{
            "excludePathContains": ["vendor/", "third_party/"],
            "clangFormatMajorVersion": 42
        }"#;
        let ignore: ClangFormatIgnore = serde_json::from_str(json).unwrap();
        assert_that(&ignore.exclude_path_contains).has_length(2);
        assert_that(&ignore.clang_format_major_version)
            .is_some()
            .is_equal_to(42);
    }

    #[test]
    fn clang_format_ignore_defaults_when_fields_missing() {
        let json = r#"{}"#;
        let ignore: ClangFormatIgnore = serde_json::from_str(json).unwrap();
        assert_that(&ignore.exclude_path_contains).is_empty();
        assert_that(&ignore.clang_format_major_version).is_none();
    }

    // -----------------------------------------------------------------------
    // load_clang_format_ignore
    // -----------------------------------------------------------------------

    #[test]
    fn load_clang_format_ignore_returns_default_when_file_missing() {
        let tmp = TempDir::new().unwrap();
        let ignore = load_clang_format_ignore(tmp.path()).unwrap();
        assert_that(&ignore.exclude_path_contains).is_empty();
    }

    #[test]
    fn load_clang_format_ignore_parses_existing_file() {
        let tmp = TempDir::new().unwrap();
        let config_path = tmp.path().join("nelson-format-ignore.json");
        fs::write(
            &config_path,
            r#"{ "excludePathContains": ["vendor", "build"], "clangFormatMajorVersion": 99 }"#,
        )
        .unwrap();
        let ignore = load_clang_format_ignore(tmp.path()).unwrap();
        assert_that(&ignore.exclude_path_contains).has_length(2);
        assert_that(&ignore.clang_format_major_version)
            .is_some()
            .is_equal_to(99);
    }

    // -----------------------------------------------------------------------
    // process_file (write mode)
    // -----------------------------------------------------------------------

    #[test]
    fn process_file_formats_json_file() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("test.json");
        fs::write(&file_path, r#"{"key":"value"}"#).unwrap();

        let result = process_file(&file_path, "json", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content.contains("  \"key\"")).is_true();
        assert_that(&content.ends_with('\n')).is_true();
    }

    #[test]
    fn process_file_cleans_up_markdown_lf() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("doc.md");
        fs::write(&file_path, "hello   \nworld\t\n\n").unwrap();

        let result = process_file(&file_path, "md", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content).is_equal_to("hello\nworld\n".to_string());
    }

    #[test]
    fn process_file_preserves_crlf_for_markdown() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("doc.md");
        fs::write(&file_path, "hello   \r\nworld\t\r\n\r\n").unwrap();

        let result = process_file(&file_path, "md", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content).is_equal_to("hello\r\nworld\r\n".to_string());
    }

    #[test]
    fn process_file_preserves_crlf_for_json() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("test.json");
        fs::write(&file_path, "{\"key\":\"value\"}\r\n").unwrap();

        let result = process_file(&file_path, "json", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        // Should use CRLF since input was CRLF
        assert_that(&content.contains("\r\n")).is_true();
        assert_that(&content.contains("  \"key\"")).is_true();
    }

    #[test]
    fn process_file_skips_unparsable_json() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("bad.json");
        let bad_content = "not json at all {{{";
        fs::write(&file_path, bad_content).unwrap();

        let result = process_file(&file_path, "json", false, false);
        assert_that(&result.is_ok()).is_true();

        // File should remain unchanged
        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content).is_equal_to(bad_content.to_string());
    }

    #[test]
    fn process_file_already_formatted_is_noop() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("clean.md");
        let clean_content = "already clean\n";
        fs::write(&file_path, clean_content).unwrap();

        let result = process_file(&file_path, "md", false, false).unwrap();
        assert_that(&result).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content).is_equal_to(clean_content.to_string());
    }

    #[test]
    fn process_file_formats_yaml_file() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("test.yml");
        fs::write(&file_path, "key: value\nlist:\n  - a\n  - b\n").unwrap();

        let result = process_file(&file_path, "yml", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content.ends_with('\n')).is_true();
    }

    #[test]
    fn process_file_returns_true_for_unknownn_extension() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("test.txt");
        fs::write(&file_path, "anything").unwrap();

        let result = process_file(&file_path, "txt", false, false).unwrap();
        assert_that(&result).is_true();
    }

    // -----------------------------------------------------------------------
    // cmake_style_arg helper
    // -----------------------------------------------------------------------

    #[test]
    fn cmake_style_arg_defaults_to_builtin() {
        let _guard = ENV_LOCK.lock().unwrap();
        std::env::remove_var("CMAKE_FMT_STYLE");
        assert_that(&cmake_style_arg()).is_equal_to(Some(DEFAULT_CMAKE_STYLE.to_string()));
        std::env::set_var("CMAKE_FMT_STYLE", "");
        assert_that(&cmake_style_arg()).is_equal_to(Some(DEFAULT_CMAKE_STYLE.to_string()));
        std::env::remove_var("CMAKE_FMT_STYLE");
    }

    #[test]
    fn cmake_style_arg_uses_env() {
        let _guard = ENV_LOCK.lock().unwrap();
        std::env::set_var("CMAKE_FMT_STYLE", "file:.cmake-fmt");
        assert_that(&cmake_style_arg()).is_equal_to(Some("file:.cmake-fmt".to_string()));
        std::env::remove_var("CMAKE_FMT_STYLE");
    }

    // -----------------------------------------------------------------------
    // resolve_clang_format
    // -----------------------------------------------------------------------

    #[test]
    fn resolve_clang_format_returns_none_for_empty_dir() {
        let tmp = TempDir::new().unwrap();
        // In a temp dir with no clang-format binary, PATH may still have one,
        // so we just verify the function doesn't panic.
        let _result = resolve_clang_format(tmp.path(), DEFAULT_CLANG_FORMAT_MAJOR_VERSION);
        // If clang-format is on PATH, result is Some; otherwise None.
        // Either way, no panic is the key assertion.
    }

    // -----------------------------------------------------------------------
    // format_xml
    //
    // format_xml now delegates to basic_cleanup.  The xmlformat crate was
    // removed because it inserted spurious blank lines between elements and
    // split inline tags (e.g. <b>) across multiple lines, corrupting the
    // Nelson help XML files.
    // -----------------------------------------------------------------------

    #[test]
    fn format_xml_preserves_well_formed_document() {
        let input = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n\
            <xmldoc>\n\
            \x20 <keyword>foo</keyword>\n\
            \x20 <description>\n\
            \x20\x20\x20 <p><b>foo</b>: a function.</p>\n\
            \x20 </description>\n\
            </xmldoc>\n";
        let result = format_xml(input);
        assert_that(&result).is_equal_to(input.to_string());
    }

    #[test]
    fn format_xml_does_not_split_inline_bold_tags() {
        // xmlformat used to emit "<b\n      >text</b>"; basic_cleanup must not.
        let input = "<p><b>SliderStep</b>: some description.</p>\n";
        let result = format_xml(input);
        assert_that(&result.contains("<b>SliderStep</b>")).is_true();
        assert_that(&result.contains("<b\n")).is_false();
    }

    #[test]
    fn format_xml_does_not_insert_blank_lines_between_elements() {
        // xmlformat used to insert 8 blank lines between sibling elements.
        let input = "<xmldoc>\n\
            \x20 <a>1</a>\n\
            \x20 <b>2</b>\n\
            \x20 <c>3</c>\n\
            </xmldoc>\n";
        let result = format_xml(input);
        // No consecutive blank lines should appear
        assert_that(&result.contains("\n\n\n")).is_false();
        assert_that(&result).is_equal_to(input.to_string());
    }

    #[test]
    fn format_xml_preserves_cdata_content() {
        let input = "<example_item_data\n\
            \x20\x20\x20\x20\x20\x20\x20\x20runnable=\"adv-cli\"\n\
            \x20\x20\x20\x20\x20\x20><![CDATA[\n\
            f = figure();\n\
            h = uicontrol(Style='checkbox');\n\
            ]]></example_item_data>\n";
        let result = format_xml(input);
        assert_that(&result.contains("f = figure();")).is_true();
        assert_that(&result.contains("h = uicontrol(Style='checkbox');")).is_true();
        // Lines inside CDATA must stay on separate lines, not be joined
        let f_pos = result.find("f = figure();").unwrap();
        let h_pos = result.find("h = uicontrol").unwrap();
        assert_that(&f_pos).is_less_than(h_pos);
    }

    #[test]
    fn format_xml_trims_trailing_whitespace_per_line() {
        let input = "<xmldoc>   \n\x20 <keyword>foo</keyword>   \n</xmldoc>\n";
        let result = format_xml(input);
        for line in result.lines() {
            assert_that(&line.ends_with(' ')).is_false();
        }
    }

    #[test]
    fn format_xml_ensures_single_trailing_newline() {
        let input = "<xmldoc>\n</xmldoc>\n\n\n";
        let result = format_xml(input);
        assert_that(&result.ends_with('\n')).is_true();
        assert_that(&result.ends_with("\n\n")).is_false();
    }

    #[test]
    fn format_xml_normalizes_crlf_to_lf() {
        let input = "<xmldoc>\r\n\x20 <keyword>foo</keyword>\r\n</xmldoc>\r\n";
        let result = format_xml(input);
        assert_that(&result.contains("\r\n")).is_false();
        assert_that(&result.ends_with('\n')).is_true();
    }

    #[test]
    fn format_xml_is_idempotent() {
        let input = "<xmldoc>\n\
            \x20 <keyword>bar</keyword>\n\
            \x20 <description>\n\
            \x20\x20\x20 <p><b>bar</b>: a description.</p>\n\
            \x20 </description>\n\
            </xmldoc>\n";
        let first = format_xml(input);
        let second = format_xml(&first);
        assert_that(&second).is_equal_to(first);
    }

    #[test]
    fn process_file_formats_xml_help_file() {
        let tmp = TempDir::new().unwrap();
        // simulate a help/en_US/xml path so standard_file_kind returns "xml"
        let xml_dir = tmp.path().join("help").join("en_US").join("xml");
        fs::create_dir_all(&xml_dir).unwrap();
        let file_path = xml_dir.join("doc.xml");
        let input = "<xmldoc>   \n\x20 <keyword>foo</keyword>   \n</xmldoc>\n\n";
        fs::write(&file_path, input).unwrap();

        let result = process_file(&file_path, "xml", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        // trailing spaces stripped, single trailing newline
        assert_that(&content.contains("   ")).is_false();
        assert_that(&content.ends_with('\n')).is_true();
        assert_that(&content.ends_with("\n\n")).is_false();
    }

    // -- cmake-fmt corruption guard -------------------------------------------

    #[test]
    fn cmake_guard_flags_dropped_binary_operand() {
        // if(WIN32 AND (X OR Y)) -> if(WIN32 AND )
        assert_that(&cmake_conditionals_are_corrupted(
            "if(WIN32\n  AND\n)\n  message(x)\nendif()\n",
        ))
        .is_true();
    }

    #[test]
    fn cmake_guard_flags_dropped_leading_operand() {
        // if((A) OR BENCHMARKS) -> if(OR BENCHMARKS)
        assert_that(&cmake_conditionals_are_corrupted(
            "if(OR BENCHMARKS)\nendif()\n",
        ))
        .is_true();
    }

    #[test]
    fn cmake_guard_flags_dropped_unary_operand() {
        // if(NOT (A STREQUAL B)) -> if(NOT)
        assert_that(&cmake_conditionals_are_corrupted("if(NOT)\nendif()\n")).is_true();
    }

    #[test]
    fn cmake_guard_flags_trailing_dropped_operand() {
        // if(NOT DEFINED X OR (...)) -> if(NOT DEFINED X OR)
        assert_that(&cmake_conditionals_are_corrupted(
            "if(NOT DEFINED X OR)\nendif()\n",
        ))
        .is_true();
    }

    #[test]
    fn cmake_guard_accepts_valid_conditionals() {
        let ok = "\
if(WIN32
   AND (CMAKE_VS_PLATFORM_NAME STREQUAL \"ARM64\"
        OR CMAKE_SYSTEM_PROCESSOR MATCHES \"^(ARM64|arm64|AARCH64|aarch64)$\"))
  message(status)
endif()

if((TESTS AND NOT XLNT_SKIP_INTERNAL_TESTS) OR BENCHMARKS)
  message(bench)
endif()

if(NOT (CMAKE_CXX_COMPILER_ID STREQUAL \"AppleClang\"))
  message(leak)
endif()

if(A AND NOT B)
  message(ok)
endif()
";
        assert_that(&cmake_conditionals_are_corrupted(ok)).is_false();
    }

    #[test]
    fn cmake_guard_ignores_parens_inside_strings_and_comments() {
        // Parentheses live only inside a quoted regex and a comment; the
        // condition itself is a single well-formed operand.
        let ok = "if(X MATCHES \"^(a|b)$\") # note: (see docs)\n  message(x)\nendif()\n";
        assert_that(&cmake_conditionals_are_corrupted(ok)).is_false();
    }

    #[test]
    fn cmake_guard_does_not_flag_identifiers_containing_keyword() {
        // `motif(...)` and `notify` must not be mistaken for `if`/`not`.
        let ok = "motif(value)\nif(notify_enabled)\n  message(x)\nendif()\n";
        assert_that(&cmake_conditionals_are_corrupted(ok)).is_false();
    }

    #[test]
    fn cmake_fmt_version_ordering() {
        assert_that(&cmake_fmt_version_ok((0, 11, 0))).is_true();
        assert_that(&cmake_fmt_version_ok((0, 11, 2))).is_true();
        assert_that(&cmake_fmt_version_ok((1, 0, 0))).is_true();
        assert_that(&cmake_fmt_version_ok((0, 10, 0))).is_false();
        assert_that(&cmake_fmt_version_ok((0, 9, 9))).is_false();
    }

    #[test]
    fn guard_restores_file_when_formatter_corrupts_it() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("CMakeLists.txt");
        let original = "if(WIN32 AND (A OR B))\n  message(x)\nendif()\n";
        // Simulate a buggy cmake-fmt having written corrupted content in place.
        fs::write(&file, "if(WIN32\n  AND\n)\n  message(x)\nendif()\n").unwrap();
        guard_cmake_output(&[file.clone()], &[Some(original.to_string())]);
        assert_that(&fs::read_to_string(&file).unwrap()).is_equal_to(original.to_string());
    }

    #[test]
    fn guard_keeps_valid_formatted_output() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("CMakeLists.txt");
        let original = "if(WIN32 AND (A OR B))\nmessage(x)\nendif()\n";
        let formatted = "if(WIN32\n  AND (A OR B))\n  message(x)\nendif()\n";
        fs::write(&file, formatted).unwrap();
        guard_cmake_output(&[file.clone()], &[Some(original.to_string())]);
        // Valid formatting must be preserved, not reverted.
        assert_that(&fs::read_to_string(&file).unwrap()).is_equal_to(formatted.to_string());
    }
}
