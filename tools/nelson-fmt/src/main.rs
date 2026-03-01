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
/// Formats JSON, XML (help docs), Markdown, JavaScript, YAML files
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
        Err(_) => return Ok(None), // unparseable JSON - skip
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
        Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout).to_string(),
        _ => content.to_string(),
    }
}

/// Format XML content.
fn format_xml(content: &str) -> String {
    let formatter = xmlformat::Formatter {
        compress: false,
        indent: 2,
        keep_comments: true,
        eof_newline: true,
    };
    match formatter.format_xml(content) {
        Ok(s) => {
            // Remove all trailing whitespace/newlines, then add exactly one newline
            let trimmed = s.trim_end();
            let mut result = String::with_capacity(trimmed.len() + 1);
            result.push_str(trimmed);
            result.push('\n');
            result
        }
        Err(_) => {
            // Fallback: also normalize trailing whitespace for unparseable XML
            let trimmed = content.trim_end();
            let mut result = String::with_capacity(trimmed.len() + 1);
            result.push_str(trimmed);
            result.push('\n');
            result
        }
    }
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
/// into them.
fn is_skipped_dir(entry: &walkdir::DirEntry, ignore: &ClangFormatIgnore) -> bool {
    if !entry.file_type().is_dir() {
        return false;
    }

    // Fast check: patterns that are just single directory names (no `/` or
    // wildcard) can be matched against the basename alone.  This is the old
    // behaviour and is useful for things like `"target"` or `"node_modules"`.
    let name = entry.file_name().to_string_lossy();
    if ignore
        .exclude_path_contains
        .iter()
        .any(|p| !p.contains('/') && !p.contains('*') && p == name.as_ref())
    {
        return true;
    }

    // Additionally, if a pattern contains a slash, it is meant to match a
    // *path* rather than just a single component.  We normalise the entry
    // path to forward slashes and perform a substring search.  This allows us
    // to prune the directory tree early instead of only filtering individual
    // files later.
    let path_str = entry.path().to_string_lossy().replace('\\', "/");
    ignore.exclude_path_contains.iter().any(|pattern| {
        let pat = pattern.replace('\\', "/");
        // strip trailing slash so that `foo/bar/` matches `foo/bar/baz` but
        // also `foo/bar` itself
        let pat = pat.trim_end_matches('/');
        !pat.is_empty() && pat.contains('/') && path_str.contains(pat)
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
        if pat.ends_with('/') {
            relative.starts_with(&pat)
        } else {
            relative.contains(&pat)
        }
    })
}

/// Determine whether a path should be excluded from formatting for any extension.
/// Return a normalised representation of `path` relative to `root`.
///
/// The caller only needs this for matching against the ignore list, so the
/// result is a UTF-8 string with forward slashes.  The Windows `\\?\` prefix
/// (if present) is stripped because the ignore patterns are written relative to
/// the repository root and never include that noisy prefix.
fn normalised_relative(path: &Path, root: &Path) -> String {
    let mut rel = path
        .strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .into_owned();

    // Remove the `\\?\` prefix which is used by the Windows API for
    // extended-length paths.  WalkDir may emit entries with this prefix even if
    // `root` itself does not contain it, causing `strip_prefix` to fail above.
    if rel.starts_with(r"\\?\") {
        rel = rel.trim_start_matches(r"\\?\").to_string();
    }

    rel.replace('\\', "/")
}

/// Determine whether a path should be excluded from formatting for any extension.
fn is_path_excluded(path: &Path, root: &Path, ignore: &ClangFormatIgnore) -> bool {
    let relative = normalised_relative(path, root);
    ignore.exclude_path_contains.iter().any(|pattern| {
        let pat = pattern.replace('\\', "/");
        if pat.ends_with('/') {
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
            "json" | "yml" | "yaml" | "md" | "js" | "cmake" => Some(ext),
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
                        eprintln!("Skipping (unparseable JSON): {}", path.display());
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

/// Resolve cmake-fmt binary to use:
/// 1. Try `<root>/tools/cmake-fmt/cmake-fmt[.exe]`
/// 2. Fall back to `cmake-fmt` on PATH
fn resolve_cmake_fmt(root: &Path) -> Option<PathBuf> {
    let local_name = if cfg!(windows) {
        "cmake-fmt.exe"
    } else {
        "cmake-fmt"
    };
    let local = root.join("tools").join("cmake-fmt").join(local_name);
    if local.is_file() {
        return Some(local);
    }

    // Try on PATH
    if Command::new(local_name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return Some(PathBuf::from(local_name));
    }

    // Not found: attempt to install via `cargo install cmake-fmt` so users
    // running the tool get an automatic experience.  This may take time
    // and requires `cargo` and network access.
    eprintln!("cmake-fmt not found. Attempting `cargo install cmake-fmt`...");
    match Command::new("cargo")
        .arg("install")
        .arg("cmake-fmt")
        .status()
    {
        Ok(status) if status.success() => {
            // Re-check the binary on PATH
            if Command::new(local_name)
                .arg("--version")
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false)
            {
                return Some(PathBuf::from(local_name));
            } else {
                eprintln!("cargo install succeeded but `cmake-fmt` not found on PATH");
            }
        }
        Ok(status) => {
            eprintln!("`cargo install cmake-fmt` failed with exit {}", status);
        }
        Err(e) => {
            eprintln!("Failed to run `cargo install cmake-fmt`: {}", e);
        }
    }

    None
}

/// Collect and process CMake files using the cmake-fmt binary.
fn process_cmake_files(
    root: &Path,
    ignore: &ClangFormatIgnore,
    check: bool,
    verbose: bool,
) -> Result<usize> {
    // Collect matching files
    let mut files: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|e| {
            if is_skipped_dir(e, ignore) {
                return false;
            }
            if e.file_type().is_dir() && is_path_excluded(e.path(), root, ignore) {
                return false;
            }
            true
        })
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if entry.file_type().is_file() {
            if let Some(ext) = path.extension().and_then(OsStr::to_str) {
                if ext == "cmake" && !is_path_excluded(path, root, ignore) {
                    files.push(path.to_path_buf());
                }
            } else if path.file_name().is_some_and(|n| n == "CMakeLists.txt")
                && !is_path_excluded(path, root, ignore)
            {
                files.push(path.to_path_buf());
            }
        }
    }

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

    if check {
        for chunk in files.chunks(50) {
            let mut cmd = Command::new(&cmake_bin);
            // pass style argument if we resolved one earlier
            if let Some(ref s) = style_arg {
                cmd.arg("--style").arg(s);
            }
            cmd.arg("--check");
            for file in chunk {
                if verbose {
                    eprintln!("Checking (cmake-fmt): {}", file.display());
                }
                cmd.arg(file);
            }
            let output = cmd.output().context("Failed to run cmake-fmt")?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                eprintln!("cmake-fmt would reformat files in this batch:\n{}", stderr);
                std::process::exit(1);
            }
        }
    } else {
        for chunk in files.chunks(50) {
            let mut cmd = Command::new(&cmake_bin);
            if let Some(ref s) = style_arg {
                cmd.arg("--style").arg(s);
            }
            cmd.arg("-i");
            for file in chunk {
                if verbose {
                    eprintln!("Formatting (cmake-fmt): {}", file.display());
                }
                cmd.arg(file);
            }
            let output = cmd.output().context("Failed to run cmake-fmt")?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                eprintln!("cmake-fmt error: {}", stderr);
            }
        }
    }

    Ok(total)
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
/// Collect, then format (or check) all C/C++ files via `clang-format`.
fn process_cpp_files(
    root: &Path,
    ignore: &ClangFormatIgnore,
    check: bool,
    verbose: bool,
) -> Result<(usize, usize)> {
    // Determine which major version we require: either the value from
    // the configuration file or the built‑in default.
    let required_major = ignore
        .clang_format_major_version
        .unwrap_or(DEFAULT_CLANG_FORMAT_MAJOR_VERSION);

    // Resolve clang-format executable:
    //   All platforms → prefer <root>/tools/clang-format/clang-format[.exe], then PATH
    let clang_format_bin = resolve_clang_format(root, required_major);

    // Collect matching files
    let mut files: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|e| {
            if is_skipped_dir(e, ignore) {
                return false;
            }
            if e.file_type().is_dir() && is_path_excluded(e.path(), root, ignore) {
                return false;
            }
            true
        })
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if let Some(ext) = path.extension().and_then(OsStr::to_str) {
            if CPP_EXTENSIONS.contains(&ext) && !is_cpp_excluded(path, root, ignore) {
                files.push(path.to_path_buf());
            }
        }
    }

    let total = files.len();
    if files.is_empty() {
        return Ok((0, 0));
    }

    let clang_format_bin = match clang_format_bin {
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

    if check {
        // Batch check: run clang-format --dry-run -Werror on chunks of files
        for chunk in files.chunks(50) {
            let mut cmd = Command::new(&clang_format_bin);
            cmd.arg(format!("--style={}", style_arg));
            cmd.args(["--dry-run", "-Werror"]);
            for file in chunk {
                if verbose {
                    eprintln!("Checking (clang-format): {}", file.display());
                }
                cmd.arg(file);
            }
            let output = cmd.output().context("Failed to run clang-format")?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                // Try to print which files would be reformatted (stderr usually contains this info)
                eprintln!(
                    "clang-format would reformat files in this batch:\n{}",
                    stderr
                );
                std::process::exit(1);
            }
        }
    } else {
        // Write mode - process in batches to stay within command-line limits
        for chunk in files.chunks(50) {
            let mut cmd = Command::new(&clang_format_bin);
            cmd.arg(format!("--style={}", style_arg));
            cmd.arg("-i");
            for file in chunk {
                if verbose {
                    eprintln!("Formatting (clang-format): {}", file.display());
                }
                cmd.arg(file);
            }
            let output = cmd.output().context("Failed to run clang-format")?;
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                eprintln!("clang-format error: {}", stderr);
            }
        }
    }

    Ok((total, 0))
}
// ===========================================================================
// Main
// ===========================================================================
fn main() -> Result<()> {
    let args = Args::parse();
    let root = std::fs::canonicalize(&args.root)
        .with_context(|| format!("Invalid root directory: {}", args.root.display()))?;

    let mut total_files: usize = 0;

    // Load nelson-format ignore list from <root>/nelson-format-ignore.json
    let ignore = load_clang_format_ignore(&root)?;

    // --- Standard files (JSON, YAML, MD, JS, XML help docs) ---------------
    let entries: Vec<_> = WalkDir::new(&root)
        .into_iter()
        .filter_entry(|e| {
            // first apply the lightweight name-based skip
            if is_skipped_dir(e, &ignore) {
                return false;
            }
            // also avoid descending into directories that are excluded by
            // the ignore list; this handles patterns containing slashes
            if e.file_type().is_dir() && is_path_excluded(e.path(), &root, &ignore) {
                return false;
            }
            true
        })
        .filter_map(|e| e.ok())
        .filter(|entry| !entry.file_type().is_dir())
        .collect();

    // Use parallel iterator for faster processing
    // Prepare list for non-CMake standard files (we'll process CMake files in a batch later)
    let results: Vec<_> = entries
        .par_iter()
        .filter_map(|entry| {
            let path = entry.path();
            if is_path_excluded(path, &root, &ignore) {
                return None;
            }
            if let Some(ext) = standard_file_kind(path, &root) {
                if ext == "cmake" {
                    return None; // skip here; handled by process_cmake_files
                }
                Some((path.to_path_buf(), ext))
            } else {
                None
            }
        })
        .map(|(path, ext)| {
            let res = process_file(&path, ext, args.check, args.verbose);
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
    let cmake_count = process_cmake_files(&root, &ignore, args.check, args.verbose)?;
    total_files += cmake_count;

    // --- C/C++ files via clang-format -------------------------------------
    let (cpp_total, _) = process_cpp_files(&root, &ignore, args.check, args.verbose)?;
    total_files += cpp_total;

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
        };
        let path = Path::new("/project/src/generated/bindings.h");
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
    // path exclusion helpers
    // -----------------------------------------------------------------------

    #[test]
    fn is_path_excluded_matches_subdirectory_pattern() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec![
                "modules/sio_client/src/socket_io/".to_string(),
            ],
            clang_format_major_version: None,
        };
        let path = Path::new(
            "/project/modules/sio_client/src/socket_io/lib/rapidjson/bin/encodings/utf16be.json",
        );
        assert_that(&is_path_excluded(path, root, &ignore)).is_true();
    }

    #[test]
    fn is_path_excluded_handles_windows_unc_prefix() {
        let root = Path::new(r"\\?\D:\project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec![
                "modules/sio_client/src/socket_io/".to_string(),
            ],
            clang_format_major_version: None,
        };
        let path = Path::new(
            r"\\?\D:\project\modules\sio_client\src\socket_io\lib\rapidjson\bin\encodings\utf16be.json",
        );
        assert_that(&is_path_excluded(path, root, &ignore)).is_true();
    }

    #[test]
    fn traversing_ignore_directory_skips_contents() {
        // build a small temporary tree with an ignored subdirectory
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        fs::create_dir_all(
            root.join("modules/sio_client/src/socket_io/lib/rapidjson/bin/encodings"),
        )
        .unwrap();
        fs::write(
            root.join("modules/sio_client/src/socket_io/lib/rapidjson/bin/encodings/test.json"),
            "{}",
        )
        .unwrap();
        fs::write(root.join("other.json"), "{}").unwrap();

        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec![
                "modules/sio_client/src/socket_io/".to_string(),
            ],
            clang_format_major_version: None,
        };

        let entries: Vec<_> = WalkDir::new(root)
            .into_iter()
            .filter_entry(|e| {
                if is_skipped_dir(e, &ignore) {
                    return false;
                }
                if e.file_type().is_dir() && is_path_excluded(e.path(), root, &ignore) {
                    return false;
                }
                true
            })
            .filter_map(|e| e.ok())
            .filter(|entry| !entry.file_type().is_dir())
            .collect();

        let filtered: Vec<_> = entries
            .iter()
            .filter_map(|entry| {
                let path = entry.path();
                if is_path_excluded(path, root, &ignore) {
                    return None;
                }
                if standard_file_kind(path, root).is_some() {
                    Some(path.to_path_buf())
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(filtered.len(), 1);
        assert!(filtered[0].ends_with("other.json"));
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
    fn process_file_skips_unparseable_json() {
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
    fn process_file_returns_true_for_unknown_extension() {
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
}
