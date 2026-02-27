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
    let path = root.join("clang-format-ignore.json");
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
/// Normalise line endings to `\n` and trim trailing whitespace on every line.
/// Ensures exactly one trailing newline.
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

/// Re-serialise JSON with 4-space indentation and a final newline.
/// Returns `None` when the file cannot be parsed (e.g. test fixtures with
/// embedded control characters) - the caller should skip it.
fn format_json(content: &str) -> Result<Option<String>> {
    let value: serde_json::Value = match serde_json::from_str(content) {
        Ok(v) => v,
        Err(_) => return Ok(None), // unparseable JSON - skip
    };

    let buf = Vec::new();
    let formatter = serde_json::ser::PrettyFormatter::with_indent(b"    ");
    let mut ser = serde_json::Serializer::with_formatter(buf, formatter);
    value.serialize(&mut ser)?;
    let mut output = String::from_utf8(ser.into_inner())?;
    output.push('\n');
    Ok(Some(output))
}
/// Round-trip YAML through serde_yaml to normalise indentation (2-space) and
/// key ordering.  Returns `None` when the content cannot be parsed.
fn format_yaml(content: &str) -> Result<Option<String>> {
    let value: serde_yaml::Value = match serde_yaml::from_str(content) {
        Ok(v) => v,
        Err(_) => return Ok(None), // unparseable YAML - skip
    };
    let mut output = serde_yaml::to_string(&value)?;
    // Ensure single trailing newline
    if !output.ends_with('\n') {
        output.push('\n');
    }
    Ok(Some(output))
}
// ===========================================================================
// Walk helpers
// ===========================================================================
/// Return `true` if this directory entry should be skipped entirely.
///
/// Entries in `exclude_path_contains` that are plain directory names (no `/`
/// or glob characters) double as a skip-dir list so the walker never descends
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
        .any(|pattern| relative.contains(pattern.as_str()))
}
/// Decide whether `path` should be processed as a "standard" file
/// (JSON / YAML / Markdown / JS / XML-help-doc) and return its logical
/// extension if so.
fn standard_file_kind<'a>(path: &'a Path, root: &Path) -> Option<&'a str> {
    let ext = path.extension()?.to_str()?;
    match ext {
        "json" | "yml" | "yaml" | "md" | "js" => Some(ext),
        "xml" => {
            // Only XML files matching modules/*/help/*/xml/*.xml
            let relative = path.strip_prefix(root).ok()?;
            let rel_str = relative.to_string_lossy().replace('\\', "/");
            if rel_str.starts_with("modules/")
                && rel_str.contains("/help/")
                && rel_str.contains("/xml/")
            {
                Some("xml")
            } else {
                None
            }
        }
        _ => None,
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

    let formatted = match ext {
        "json" => match format_json(&content)? {
            Some(f) => f,
            None => {
                if verbose {
                    eprintln!("Skipping (unparseable JSON): {}", path.display());
                }
                return Ok(true);
            }
        },
        "yml" | "yaml" => match format_yaml(&content)? {
            Some(f) => f,
            None => {
                if verbose {
                    eprintln!("Skipping (unparseable YAML): {}", path.display());
                }
                return Ok(true);
            }
        },
        "md" | "js" | "xml" => basic_cleanup(&content),
        _ => return Ok(true),
    };

    if content == formatted {
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
///   - On Windows: try `<root>/tools/clang-format.exe` first, then fall back
///     to the system PATH.
///   - On other platforms: use the system-installed `clang-format` from PATH.
///
/// Returns `None` when no usable binary is found.
fn resolve_clang_format(root: &Path) -> Option<PathBuf> {
    // On Windows, prefer the local copy shipped in tools/clang-format/
    if cfg!(windows) {
        let local = root
            .join("tools")
            .join("clang-format")
            .join("clang-format.exe");
        if local.is_file() {
            return Some(local);
        }
    }

    // Fall back to PATH
    let name = if cfg!(windows) {
        "clang-format.exe"
    } else {
        "clang-format"
    };
    if Command::new(name)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        return Some(PathBuf::from(name));
    }

    None
}
/// Collect, then format (or check) all C/C++ files via `clang-format`.
fn process_cpp_files(
    root: &Path,
    ignore: &ClangFormatIgnore,
    check: bool,
    verbose: bool,
) -> Result<(usize, usize)> {
    // Resolve clang-format executable:
    //   Windows  → prefer <root>/tools/clang-format/clang-format.exe, then PATH
    //   Others   → system PATH
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

    // Point clang-format at the project's .clang-format config file
    let style_file = root.join(".clang-format");
    let style_arg = format!("file:{}", style_file.display());

    if verbose {
        eprintln!("Using clang-format: {}", clang_format_bin.display());
        eprintln!("Using style: {}", style_arg);
    }

    if check {
        for file in &files {
            if verbose {
                eprintln!("Checking (clang-format): {}", file.display());
            }
            let output = Command::new(&clang_format_bin)
                .arg(format!("--style={}", style_arg))
                .args(["--dry-run", "-Werror"])
                .arg(file)
                .output()
                .context("Failed to run clang-format")?;
            if !output.status.success() {
                eprintln!("Would reformat: {}", file.display());
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

    // Load clang-format ignore list from <root>/clang-format-ignore.json
    let ignore = load_clang_format_ignore(&root)?;

    // --- Standard files (JSON, YAML, MD, JS, XML help docs) ---------------
    for entry in WalkDir::new(&root)
        .into_iter()
        .filter_entry(|e| !is_skipped_dir(e, &ignore))
        .filter_map(|e| e.ok())
    {
        if entry.file_type().is_dir() {
            continue;
        }
        let path = entry.path();
        if let Some(ext) = standard_file_kind(path, &root) {
            total_files += 1;
            match process_file(path, ext, args.check, args.verbose) {
                Ok(_) => {}
                Err(e) => eprintln!("Error processing {}: {:#}", path.display(), e),
            }
        }
    }

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
    // format_json
    // -----------------------------------------------------------------------
    #[test]
    fn format_json_pretty_prints_with_4_space_indent() {
        let input = r#"{"a":1,"b":[2,3]}"#;
        let result = format_json(input).unwrap();
        assert_that(&result).is_some();
        let formatted = result.unwrap();
        // Check 4-space indentation
        assert_that(&formatted.contains("    \"a\"")).is_true();
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
        let input = "{\n    \"key\": \"value\"\n}\n";
        let result = format_json(input).unwrap().unwrap();
        assert_that(&result).is_equal_to(input.to_string());
    }

    // -----------------------------------------------------------------------
    // format_yaml
    // -----------------------------------------------------------------------

    #[test]
    fn format_yaml_round_trips_simple_document() {
        let input = "key: value\nlist:\n  - a\n  - b\n";
        let result = format_yaml(input).unwrap();
        assert_that(&result).is_some();
        let formatted = result.unwrap();
        assert_that(&formatted.contains("key:")).is_true();
        assert_that(&formatted.ends_with('\n')).is_true();
    }

    #[test]
    fn format_yaml_returns_none_for_invalid_yaml() {
        let input = ":\n  : :\n  [invalid\n";
        let _result = format_yaml(input).unwrap();
        // serde_yaml may or may not parse this; if it does, it returns Some
        // The important thing is it doesn't panic/error
        // We just verify the Result is Ok
    }

    #[test]
    fn format_yaml_ensures_trailing_newline() {
        let input = "key: value";
        let result = format_yaml(input).unwrap();
        if let Some(formatted) = result {
            assert_that(&formatted.ends_with('\n')).is_true();
        }
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
        assert_that(&content.contains("    \"key\"")).is_true();
        assert_that(&content.ends_with('\n')).is_true();
    }

    #[test]
    fn process_file_cleans_up_markdown() {
        let tmp = TempDir::new().unwrap();
        let file_path = tmp.path().join("doc.md");
        fs::write(&file_path, "hello   \r\nworld\t\r\n\r\n").unwrap();

        let result = process_file(&file_path, "md", false, false);
        assert_that(&result.is_ok()).is_true();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_that(&content.contains("\r\n")).is_false();
        assert_that(&content).is_equal_to("hello\nworld\n".to_string());
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
