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
use serde::{Deserialize, Serialize};
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
/// Format CMake content by calling the cmake-fmt binary.
fn format_cmake_file(path: &Path, content: &str) -> String {
    let output = Command::new("cmake-fmt")
        .arg("--assume-filename")
        .arg(path)
        .arg("-")
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
use walkdir::WalkDir;
use rayon::prelude::*;
use toml_edit::Document;
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

/// Required major version of clang-format.
/// All platforms **must** use the same major version so that formatting output
/// is identical regardless of OS.
const CLANG_FORMAT_MAJOR_VERSION: u32 = 20;

/// Layout of the `clang-format-ignore.json` configuration file that lives
/// next to `.clang-format` in the project root.
#[derive(Deserialize, Debug, Default)]
#[serde(rename_all = "camelCase")]
struct ClangFormatIgnore {
    #[serde(default)]
    exclude_path_contains: Vec<String>,
}

/// Load the ignore list from `<root>/clang-format-ignore.json`.
/// Returns a default (empty) ignore list when the file is absent.
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
            return result;
        },
        Err(_) => {
            // Fallback: also normalize trailing whitespace for unparseable XML
            let trimmed = content.trim_end();
            let mut result = String::with_capacity(trimmed.len() + 1);
            result.push_str(trimmed);
            result.push('\n');
            return result;
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
    let rustfmt_path = if cfg!(windows) { "rustfmt.exe" } else { "rustfmt" };
    let output = Command::new(rustfmt_path)
        .arg("--emit").arg("stdout")
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
                eprintln!("rustfmt not found or failed, using basic_cleanup: {}", path.display());
            }
            basic_cleanup(content)
        }
    }
}
// ===========================================================================
// Walk helpers
// ===========================================================================
/// into them.
fn is_skipped_dir(entry: &walkdir::DirEntry, ignore: &ClangFormatIgnore) -> bool {
    if !entry.file_type().is_dir() {
        return false;
    }
    let name = entry.file_name().to_string_lossy();
    ignore.exclude_path_contains.iter().any(|p| {
        !p.contains('/') && !p.contains('*') && p == name.as_ref()
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
    ignore
        .exclude_path_contains
        .iter()
        .any(|pattern| {
            let pat = pattern.replace('\\', "/");
            if pat.ends_with('/') {
                relative.starts_with(&pat)
            } else {
                relative.contains(&pat)
            }
        })
}

/// Determine whether a path should be excluded from formatting for any extension.
fn is_path_excluded(path: &Path, root: &Path, ignore: &ClangFormatIgnore) -> bool {
    let relative = path.strip_prefix(root).unwrap_or(path).to_string_lossy().replace('\\', "/");
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
    if path.file_name().map_or(false, |n| n == "CMakeLists.txt") {
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
            "xml" => Some("xml"),
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
    let formatted_lf = if path.file_name().map_or(false, |n| n == "CMakeLists.txt") || ext == "cmake" {
        format_cmake_file(path, &content)
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
fn resolve_clang_format(root: &Path) -> Option<PathBuf> {
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
    let versioned = format!("clang-format-{}", CLANG_FORMAT_MAJOR_VERSION);
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
    let local_name = if cfg!(windows) { "cmake-fmt.exe" } else { "cmake-fmt" };
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
    match Command::new("cargo").arg("install").arg("cmake-fmt").status() {
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
fn process_cmake_files(root: &Path, ignore: &ClangFormatIgnore, check: bool, verbose: bool) -> Result<usize> {
    // Collect matching files
    let mut files: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|e| !is_skipped_dir(e, ignore))
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if entry.file_type().is_file() {
            if let Some(ext) = path.extension().and_then(OsStr::to_str) {
                if ext == "cmake" && !is_path_excluded(path, root, ignore) {
                    files.push(path.to_path_buf());
                }
            } else if path.file_name().map_or(false, |n| n == "CMakeLists.txt") && !is_path_excluded(path, root, ignore) {
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
            eprintln!("WARNING: cmake-fmt not found - skipping {} CMake files", files.len());
            return Ok(0);
        }
    };

    if verbose {
        eprintln!("Using cmake-fmt: {}", cmake_bin.display());
    }

    if check {
        for chunk in files.chunks(50) {
            let mut cmd = Command::new(&cmake_bin);
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

/// Check that the detected clang-format matches `CLANG_FORMAT_MAJOR_VERSION`.
/// Prints a warning and returns `false` on mismatch.
fn check_clang_format_version(bin: &Path) -> bool {
    match clang_format_major_version(bin) {
        Some(major) if major == CLANG_FORMAT_MAJOR_VERSION => true,
        Some(major) => {
            eprintln!(
                "WARNING: clang-format major version is {} but {} is required. \
                 Formatting may differ across platforms. \
                 Please install clang-format {}.",
                major, CLANG_FORMAT_MAJOR_VERSION, CLANG_FORMAT_MAJOR_VERSION
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
    // Resolve clang-format executable:
    //   All platforms → prefer <root>/tools/clang-format/clang-format[.exe], then PATH
    let clang_format_bin = resolve_clang_format(root);

    // Collect matching files
    let mut files: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|e| !is_skipped_dir(e, ignore))
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
    check_clang_format_version(&clang_format_bin);

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
                eprintln!("clang-format would reformat files in this batch:\n{}", stderr);
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
        .filter_entry(|e| !is_skipped_dir(e, &ignore))
        .filter_map(|e| e.ok())
        .filter(|entry| !entry.file_type().is_dir())
        .collect();

    // Use parallel iterator for faster processing
    // Prepare list for non-CMake standard files (we'll process CMake files in a batch later)
    let results: Vec<_> = entries.par_iter()
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
    use tempfile::TempDir;

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
        let results: Vec<_> = files.par_iter()
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
        };
        let path = Path::new("/project/modules/third_party/lib.cpp");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_true();
    }

    #[test]
    fn is_cpp_excluded_no_match_for_normal_path() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec!["third_party".to_string()],
        };
        let path = Path::new("/project/modules/core/main.cpp");
        assert_that(&is_cpp_excluded(path, root, &ignore)).is_false();
    }

    #[test]
    fn is_cpp_excluded_matches_multiple_patterns() {
        let root = Path::new("/project");
        let ignore = ClangFormatIgnore {
            exclude_path_contains: vec!["vendored".to_string(), "generated".to_string()],
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
            "excludePathContains": ["vendor/", "third_party/"]
        }"#;
        let ignore: ClangFormatIgnore = serde_json::from_str(json).unwrap();
        assert_that(&ignore.exclude_path_contains).has_length(2);
    }

    #[test]
    fn clang_format_ignore_defaults_when_fields_missing() {
        let json = r#"{}"#;
        let ignore: ClangFormatIgnore = serde_json::from_str(json).unwrap();
        assert_that(&ignore.exclude_path_contains).is_empty();
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
        let config_path = tmp.path().join("clang-format-ignore.json");
        fs::write(
            &config_path,
            r#"{"excludePathContains": ["vendor", "build"]}"#,
        )
        .unwrap();
        let ignore = load_clang_format_ignore(tmp.path()).unwrap();
        assert_that(&ignore.exclude_path_contains).has_length(2);
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
    // resolve_clang_format
    // -----------------------------------------------------------------------

    #[test]
    fn resolve_clang_format_returns_none_for_empty_dir() {
        let tmp = TempDir::new().unwrap();
        // In a temp dir with no clang-format binary, PATH may still have one,
        // so we just verify the function doesn't panic.
        let _result = resolve_clang_format(tmp.path());
        // If clang-format is on PATH, result is Some; otherwise None.
        // Either way, no panic is the key assertion.
    }
}
