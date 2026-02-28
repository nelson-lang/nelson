// ==============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ==============================================================================
// This file is part of Nelson.
// =============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ==============================================================================
use anyhow::{bail, Context, Result};
use chrono::Local;
use clap::Parser;
use regex::Regex;
use serde::Serialize;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use walkdir::WalkDir;

/// Update Nelson version across all project files.
///
/// When GITHUB_SHA and GITHUB_RUN_NUMBER environment variables are set,
/// version is read from package.json and build number comes from CI.
/// Otherwise, all four version components must be provided as arguments.
#[derive(Parser, Debug)]
#[command(
    name = "nelson-update-version",
    about = "Update Nelson version numbers across the project"
)]
struct Args {
    /// Major version number
    major: Option<u32>,
    /// Minor version number
    minor: Option<u32>,
    /// Maintenance version number
    maintenance: Option<u32>,
    /// Build version number
    build: Option<u32>,
}

// ---------------------------------------------------------------------------
// Git helpers
// ---------------------------------------------------------------------------

fn is_dirty_git() -> Result<bool> {
    let output = Command::new("git")
        .args(["status", "-uno", "--porcelain"])
        .output()
        .context("Failed to run git status")?;
    let status = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(!status.is_empty())
}

fn get_git_revision_hash() -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .context("Failed to get git revision hash")?;
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

// ---------------------------------------------------------------------------
// GitHub CI helpers
// ---------------------------------------------------------------------------

fn get_github_repo_commit() -> Option<String> {
    env::var("GITHUB_SHA").ok()
}

fn get_github_build_number() -> Option<u32> {
    env::var("GITHUB_RUN_NUMBER")
        .ok()
        .and_then(|s| s.parse().ok())
}

fn use_github_variables() -> bool {
    get_github_repo_commit().is_some() && get_github_build_number().is_some()
}

// ---------------------------------------------------------------------------
// Version reading
// ---------------------------------------------------------------------------
fn get_current_version(root: &Path) -> Result<(u32, u32, u32)> {
    let content =
        fs::read_to_string(root.join("package.json")).context("Failed to read package.json")?;
    let data: serde_json::Value =
        serde_json::from_str(&content).context("Failed to parse package.json")?;
    let version_str = data["version"]
        .as_str()
        .context("Missing 'version' field in package.json")?;
    let parts: Vec<&str> = version_str.split('.').collect();
    if parts.len() != 3 {
        bail!("Invalid version format in package.json: {}", version_str);
    }
    Ok((parts[0].parse()?, parts[1].parse()?, parts[2].parse()?))
}

// ---------------------------------------------------------------------------
// File editors
// ---------------------------------------------------------------------------
/// Rewrite a Windows .rc resource file, updating version fields.
fn edit_rc_file(path: &Path, version_str: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;
    let mut lines_out: Vec<String> = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("FILEVERSION") {
            lines_out.push(format!(" FILEVERSION {}", version_str));
        } else if trimmed.starts_with("PRODUCTVERSION") {
            lines_out.push(format!(" PRODUCTVERSION {}", version_str));
        } else if trimmed.starts_with("VALUE \"ProductVersion\", \"") {
            lines_out.push(format!(
                "            VALUE \"ProductVersion\", \"{}\"",
                version_str
            ));
        } else if trimmed.starts_with("VALUE \"FileVersion\", \"") {
            lines_out.push(format!(
                "            VALUE \"FileVersion\", \"{}\"",
                version_str
            ));
        } else {
            lines_out.push(line.to_string());
        }
    }
    fs::write(path, lines_out.join("\n") + "\n")?;
    Ok(())
}

/// Walk `<root>/modules` and update every `.rc` file.
fn edit_rc_files_in_modules(
    root: &Path,
    major: u32,
    minor: u32,
    maintenance: u32,
    build: u32,
) -> Result<()> {
    let version_str = format!("{},{},{},{}", major, minor, maintenance, build);
    for entry in WalkDir::new(root.join("modules"))
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "rc") {
            edit_rc_file(path, &version_str)?;
        }
    }
    Ok(())
}

/// Update the four `Nelson_VERSION_*_DEFAULT` variables in `CMakeLists.txt`.
fn edit_cmakelist(root: &Path, major: u32, minor: u32, maintenance: u32, build: u32) -> Result<()> {
    let filename = root.join("CMakeLists.txt");
    let content = fs::read_to_string(&filename)?;
    let mut lines_out: Vec<String> = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("set(Nelson_VERSION_MAJOR_DEFAULT") {
            lines_out.push(format!("set(Nelson_VERSION_MAJOR_DEFAULT {})", major));
        } else if trimmed.starts_with("set(Nelson_VERSION_MINOR_DEFAULT") {
            lines_out.push(format!("set(Nelson_VERSION_MINOR_DEFAULT {})", minor));
        } else if trimmed.starts_with("set(Nelson_VERSION_MAINTENANCE_DEFAULT") {
            lines_out.push(format!(
                "set(Nelson_VERSION_MAINTENANCE_DEFAULT {})",
                maintenance
            ));
        } else if trimmed.starts_with("set(Nelson_VERSION_BUILD_DEFAULT") {
            lines_out.push(format!("set(Nelson_VERSION_BUILD_DEFAULT {})", build));
        } else {
            lines_out.push(line.to_string());
        }
    }
    fs::write(&filename, lines_out.join("\n") + "\n")?;
    Ok(())
}

/// Delete `Nelson_VERSION.h` if it exists (generated file).
fn delete_nelson_version_h(root: &Path) -> Result<()> {
    let filename = root.join("modules/commons/src/include/Nelson_VERSION.h");
    if filename.is_file() {
        fs::remove_file(&filename)?;
    }
    Ok(())
}

/// Update the `#define` directives in `Nelson_VERSION.h.vc`.
fn edit_nelson_version_h_vc(
    root: &Path,
    major: u32,
    minor: u32,
    maintenance: u32,
    build: u32,
    git_hash: &str,
) -> Result<()> {
    let filename = root.join("modules/commons/src/include/Nelson_VERSION.h.vc");
    let content = fs::read_to_string(&filename)?;
    let mut lines_out: Vec<String> = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("#define NELSON_VERSION_COMMIT_HASH") {
            lines_out.push(format!(
                "#define NELSON_VERSION_COMMIT_HASH \"{}\"",
                git_hash
            ));
        } else if trimmed.starts_with("#define NELSON_VERSION_MAJOR") {
            lines_out.push(format!("#define NELSON_VERSION_MAJOR {}", major));
        } else if trimmed.starts_with("#define NELSON_VERSION_MINOR") {
            lines_out.push(format!("#define NELSON_VERSION_MINOR {}", minor));
        } else if trimmed.starts_with("#define NELSON_VERSION_MAINTENANCE") {
            lines_out.push(format!(
                "#define NELSON_VERSION_MAINTENANCE {}",
                maintenance
            ));
        } else if trimmed.starts_with("#define NELSON_VERSION_BUILD") {
            lines_out.push(format!("#define NELSON_VERSION_BUILD {}", build));
        } else {
            lines_out.push(line.to_string());
        }
    }
    fs::write(&filename, lines_out.join("\n") + "\n")?;
    Ok(())
}

/// Update the commit hash in `Nelson_VERSION.h.in`.
fn edit_nelson_version_h_in(root: &Path, git_hash: &str) -> Result<()> {
    let filename = root.join("modules/commons/src/include/Nelson_VERSION.h.in");
    let content = fs::read_to_string(&filename)?;
    let mut lines_out: Vec<String> = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("#define NELSON_VERSION_COMMIT_HASH") {
            lines_out.push(format!(
                "#define NELSON_VERSION_COMMIT_HASH \"{}\"",
                git_hash
            ));
        } else {
            lines_out.push(line.to_string());
        }
    }
    fs::write(&filename, lines_out.join("\n") + "\n")?;
    Ok(())
}

/// Rewrite `package.json` with the new semantic version.
fn edit_package_json(root: &Path, major: u32, minor: u32, maintenance: u32) -> Result<()> {
    let filename = root.join("package.json");
    let content = fs::read_to_string(&filename)?;
    let mut data: serde_json::Value = serde_json::from_str(&content)?;
    data["version"] = serde_json::Value::String(format!("{}.{}.{}", major, minor, maintenance));

    let buf = Vec::new();
    let formatter = serde_json::ser::PrettyFormatter::with_indent(b"    ");
    let mut ser = serde_json::Serializer::with_formatter(buf, formatter);
    data.serialize(&mut ser)?;
    let mut output = String::from_utf8(ser.into_inner())?;
    output.push('\n');
    fs::write(&filename, output)?;
    Ok(())
}

/// Update `[workspace.package]` `version = "x.y.z"` in the root `Cargo.toml`.
fn edit_workspace_cargo_toml(root: &Path, major: u32, minor: u32, maintenance: u32) -> Result<()> {
    let filename = root.join("Cargo.toml");
    let content = fs::read_to_string(&filename)?;
    let mut lines_out: Vec<String> = Vec::new();
    let mut in_workspace_package = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_workspace_package = trimmed == "[workspace.package]";
            lines_out.push(line.to_string());
            continue;
        }
        if in_workspace_package && trimmed.starts_with("version") {
            lines_out.push(format!("version = \"{}.{}.{}\"", major, minor, maintenance));
        } else {
            lines_out.push(line.to_string());
        }
    }
    fs::write(&filename, lines_out.join("\n") + "\n")?;
    Ok(())
}

/// Update the `### Nelson x.y.z.b` heading in every `homepage.md`.
fn edit_homepage_md(root: &Path, version_str: &str) -> Result<()> {
    for entry in WalkDir::new(root.join("modules/main/help"))
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.file_name().is_some_and(|n| n == "homepage.md") {
            let content = fs::read_to_string(path)?;
            let mut lines_out: Vec<String> = Vec::new();
            for line in content.lines() {
                if line.trim().starts_with("### Nelson ") {
                    lines_out.push(format!("### Nelson {}", version_str));
                } else {
                    lines_out.push(line.to_string());
                }
            }
            fs::write(path, lines_out.join("\n") + "\n")?;
        }
    }
    Ok(())
}

/// Update `X-Nelson-Version=` in `.desktop` files.
fn edit_desktop_files(root: &Path, major: u32, minor: u32, maintenance: u32) -> Result<()> {
    let version_str = format!("{}.{}.{}", major, minor, maintenance);
    for entry in WalkDir::new(root.join("desktop"))
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "desktop") {
            let content = fs::read_to_string(path)?;
            let mut lines_out: Vec<String> = Vec::new();
            for line in content.lines() {
                if line.trim().starts_with("X-Nelson-Version=") {
                    lines_out.push(format!("X-Nelson-Version={}", version_str));
                } else {
                    lines_out.push(line.to_string());
                }
            }
            fs::write(path, lines_out.join("\n") + "\n")?;
        }
    }
    Ok(())
}

/// Update `<release version=… date=…>` in `.appdata.xml` files.
fn edit_appdata_files(root: &Path, major: u32, minor: u32, maintenance: u32) -> Result<()> {
    let version_str = format!("{}.{}.{}", major, minor, maintenance);
    let date_str = Local::now().format("%Y-%m-%d").to_string();
    let version_re = Regex::new(r#"version="[^"]*""#)?;
    let date_re = Regex::new(r#"date="[^"]*""#)?;

    for entry in WalkDir::new(root.join("desktop"))
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "xml")
            && path.to_string_lossy().contains("appdata")
        {
            let content = fs::read_to_string(path)?;
            let mut lines_out: Vec<String> = Vec::new();
            for line in content.lines() {
                if line.contains("<release ") && line.contains("version=") {
                    let mut s = version_re
                        .replace(line, format!("version=\"{}\"", version_str))
                        .to_string();
                    if s.contains("date=") {
                        s = date_re
                            .replace(&s, format!("date=\"{}\"", date_str))
                            .to_string();
                    } else {
                        s = s.replace("<release ", &format!("<release date=\"{}\" ", date_str));
                    }
                    lines_out.push(s);
                } else {
                    lines_out.push(line.to_string());
                }
            }
            fs::write(path, lines_out.join("\n") + "\n")?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() -> Result<()> {
    let root = Path::new(".");
    let major: u32;
    let minor: u32;
    let maintenance: u32;
    let build: u32;
    let git_hash: String;
    let update_from_command_line: bool;

    if use_github_variables() {
        println!("USE GITHUB");
        let commit = get_github_repo_commit().unwrap();
        let build_num = get_github_build_number().unwrap();
        println!("REPO COMMIT: {}", commit);
        println!("BUILD NUMBER: {}", build_num);

        let current = get_current_version(root)?;
        major = current.0;
        minor = current.1;
        maintenance = current.2;
        build = build_num;
        git_hash = commit;
        update_from_command_line = false;
    } else {
        println!("USE COMMAND LINE");
        let args = Args::parse();
        major = args.major.context("major version required")?;
        minor = args.minor.context("minor version required")?;
        maintenance = args.maintenance.context("maintenance version required")?;
        build = args.build.context("build version required")?;

        let mut hash = get_git_revision_hash()?;
        if is_dirty_git()? {
            hash = format!("{}_dirty", hash);
            println!("WARNING: dirty version detected.");
        }
        git_hash = hash;
        update_from_command_line = true;
    }

    let version_str = format!("{}.{}.{}.{}", major, minor, maintenance, build);
    println!("VERSION: {}", version_str);
    println!("HASH: {}", git_hash);

    edit_rc_files_in_modules(root, major, minor, maintenance, build)?;
    edit_cmakelist(root, major, minor, maintenance, build)?;
    delete_nelson_version_h(root)?;
    edit_nelson_version_h_vc(root, major, minor, maintenance, build, &git_hash)?;
    edit_nelson_version_h_in(root, &git_hash)?;

    if update_from_command_line {
        edit_package_json(root, major, minor, maintenance)?;
        edit_workspace_cargo_toml(root, major, minor, maintenance)?;
    }

    edit_homepage_md(root, &version_str)?;
    edit_desktop_files(root, major, minor, maintenance)?;
    edit_appdata_files(root, major, minor, maintenance)?;

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
    // edit_rc_file
    // -----------------------------------------------------------------------

    #[test]
    fn edit_rc_file_updates_file_and_product_version() {
        let tmp = TempDir::new().unwrap();
        let rc_path = tmp.path().join("test.rc");
        let content = "\
 FILEVERSION 0,0,0,0\n\
 PRODUCTVERSION 0,0,0,0\n\
            VALUE \"FileVersion\", \"0,0,0,0\"\n\
            VALUE \"ProductVersion\", \"0,0,0,0\"\n\
other line\n";
        fs::write(&rc_path, content).unwrap();

        edit_rc_file(&rc_path, "1,2,3,4").unwrap();

        let result = fs::read_to_string(&rc_path).unwrap();
        assert_that(&result.contains("FILEVERSION 1,2,3,4")).is_true();
        assert_that(&result.contains("PRODUCTVERSION 1,2,3,4")).is_true();
        assert_that(&result.contains("VALUE \"FileVersion\", \"1,2,3,4\"")).is_true();
        assert_that(&result.contains("VALUE \"ProductVersion\", \"1,2,3,4\"")).is_true();
        assert_that(&result.contains("other line")).is_true();
    }

    #[test]
    fn edit_rc_file_preserves_non_version_lines() {
        let tmp = TempDir::new().unwrap();
        let rc_path = tmp.path().join("test.rc");
        let content = "BEGIN\n FILEVERSION 0,0,0,0\nEND\n";
        fs::write(&rc_path, content).unwrap();

        edit_rc_file(&rc_path, "9,8,7,6").unwrap();

        let result = fs::read_to_string(&rc_path).unwrap();
        assert_that(&result.contains("BEGIN")).is_true();
        assert_that(&result.contains("END")).is_true();
        assert_that(&result.contains("FILEVERSION 9,8,7,6")).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_cmakelist
    // -----------------------------------------------------------------------

    #[test]
    fn edit_cmakelist_updates_version_variables() {
        let tmp = TempDir::new().unwrap();
        let cmake_content = "\
cmake_minimum_required(VERSION 3.20)\n\
set(Nelson_VERSION_MAJOR_DEFAULT 0)\n\
set(Nelson_VERSION_MINOR_DEFAULT 0)\n\
set(Nelson_VERSION_MAINTENANCE_DEFAULT 0)\n\
set(Nelson_VERSION_BUILD_DEFAULT 0)\n\
project(Nelson)\n";
        fs::write(tmp.path().join("CMakeLists.txt"), cmake_content).unwrap();

        edit_cmakelist(tmp.path(), 2, 5, 3, 100).unwrap();

        let result = fs::read_to_string(tmp.path().join("CMakeLists.txt")).unwrap();
        assert_that(&result.contains("set(Nelson_VERSION_MAJOR_DEFAULT 2)")).is_true();
        assert_that(&result.contains("set(Nelson_VERSION_MINOR_DEFAULT 5)")).is_true();
        assert_that(&result.contains("set(Nelson_VERSION_MAINTENANCE_DEFAULT 3)")).is_true();
        assert_that(&result.contains("set(Nelson_VERSION_BUILD_DEFAULT 100)")).is_true();
        assert_that(&result.contains("project(Nelson)")).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_nelson_version_h_vc
    // -----------------------------------------------------------------------

    #[test]
    fn edit_nelson_version_h_vc_updates_defines() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("modules/commons/src/include");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("Nelson_VERSION.h.vc");
        let content = "\
#define NELSON_VERSION_MAJOR 0\n\
#define NELSON_VERSION_MINOR 0\n\
#define NELSON_VERSION_MAINTENANCE 0\n\
#define NELSON_VERSION_BUILD 0\n\
#define NELSON_VERSION_COMMIT_HASH \"000\"\n\
// other stuff\n";
        fs::write(&file, content).unwrap();

        edit_nelson_version_h_vc(tmp.path(), 1, 16, 0, 42, "abc123").unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("#define NELSON_VERSION_MAJOR 1")).is_true();
        assert_that(&result.contains("#define NELSON_VERSION_MINOR 16")).is_true();
        assert_that(&result.contains("#define NELSON_VERSION_MAINTENANCE 0")).is_true();
        assert_that(&result.contains("#define NELSON_VERSION_BUILD 42")).is_true();
        assert_that(&result.contains("#define NELSON_VERSION_COMMIT_HASH \"abc123\"")).is_true();
        assert_that(&result.contains("// other stuff")).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_nelson_version_h_in
    // -----------------------------------------------------------------------

    #[test]
    fn edit_nelson_version_h_in_updates_commit_hash() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("modules/commons/src/include");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("Nelson_VERSION.h.in");
        let content = "\
#define NELSON_VERSION_COMMIT_HASH \"old_hash\"\n\
#define NELSON_VERSION_MAJOR @Nelson_VERSION_MAJOR@\n";
        fs::write(&file, content).unwrap();

        edit_nelson_version_h_in(tmp.path(), "deadbeef").unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("#define NELSON_VERSION_COMMIT_HASH \"deadbeef\"")).is_true();
        // Other lines preserved
        assert_that(&result.contains("@Nelson_VERSION_MAJOR@")).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_package_json
    // -----------------------------------------------------------------------

    #[test]
    fn edit_package_json_updates_version() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("package.json");
        let content = r#"{
    "name": "nelson",
    "version": "0.0.0",
    "description": "test"
}
"#;
        fs::write(&file, content).unwrap();

        edit_package_json(tmp.path(), 2, 5, 3).unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("\"version\": \"2.5.3\"")).is_true();
        // Other fields preserved
        assert_that(&result.contains("\"name\": \"nelson\"")).is_true();
        assert_that(&result.contains("\"description\": \"test\"")).is_true();
    }

    #[test]
    fn edit_package_json_uses_4_space_indent() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("package.json");
        fs::write(&file, r#"{"name":"test","version":"0.0.0"}"#).unwrap();

        edit_package_json(tmp.path(), 1, 0, 0).unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("    \"name\"")).is_true();
        assert_that(&result.ends_with('\n')).is_true();
    }

    #[test]
    fn edit_workspace_cargo_toml_updates_workspace_version() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("Cargo.toml");
        let content = r#"[workspace]
members = ["tools/nelson-fmt"]

[workspace.package]
version = "0.0.0"

[package]
name = "something"
"#;
        fs::write(&file, content).unwrap();

        edit_workspace_cargo_toml(tmp.path(), 2, 5, 3).unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("[workspace.package]")).is_true();
        assert_that(&result.contains("version = \"2.5.3\""))
            .is_true();
        // Ensure other sections remain
        assert_that(&result.contains("[package]")).is_true();
    }

    // -----------------------------------------------------------------------
    // get_current_version
    // -----------------------------------------------------------------------

    #[test]
    fn get_current_version_parses_semver() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("package.json");
        fs::write(&file, r#"{"version": "1.16.0"}"#).unwrap();

        let (major, minor, maintenance) = get_current_version(tmp.path()).unwrap();
        assert_that(&major).is_equal_to(1);
        assert_that(&minor).is_equal_to(16);
        assert_that(&maintenance).is_equal_to(0);
    }

    #[test]
    fn get_current_version_errors_on_missing_field() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("package.json");
        fs::write(&file, r#"{"name": "test"}"#).unwrap();

        let result = get_current_version(tmp.path());
        assert_that(&result.is_err()).is_true();
    }

    #[test]
    fn get_current_version_errors_on_bad_format() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("package.json");
        fs::write(&file, r#"{"version": "1.2"}"#).unwrap();

        let result = get_current_version(tmp.path());
        assert_that(&result.is_err()).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_homepage_md
    // -----------------------------------------------------------------------

    #[test]
    fn edit_homepage_md_updates_version_heading() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("modules/main/help/en_US");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("homepage.md");
        fs::write(&file, "# Welcome\n### Nelson 0.0.0.0\nSome content\n").unwrap();

        edit_homepage_md(tmp.path(), "2.5.3.100").unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("### Nelson 2.5.3.100")).is_true();
        assert_that(&result.contains("# Welcome")).is_true();
        assert_that(&result.contains("Some content")).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_desktop_files
    // -----------------------------------------------------------------------

    #[test]
    fn edit_desktop_files_updates_version() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("desktop");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("nelson.desktop");
        let content = "\
[Desktop Entry]\n\
Name=Nelson\n\
X-Nelson-Version=0.0.0\n\
Exec=nelson\n";
        fs::write(&file, content).unwrap();

        edit_desktop_files(tmp.path(), 2, 5, 3).unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("X-Nelson-Version=2.5.3")).is_true();
        assert_that(&result.contains("Name=Nelson")).is_true();
        assert_that(&result.contains("Exec=nelson")).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_appdata_files
    // -----------------------------------------------------------------------

    #[test]
    fn edit_appdata_files_updates_version_and_date() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("desktop");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("io.github.nelson_lang.Nelson.appdata.xml");
        let content = r#"<?xml version="1.0"?>
<component>
  <releases>
    <release version="0.0.0" date="2000-01-01"/>
  </releases>
</component>
"#;
        fs::write(&file, content).unwrap();

        edit_appdata_files(tmp.path(), 2, 5, 3).unwrap();

        let result = fs::read_to_string(&file).unwrap();
        assert_that(&result.contains("version=\"2.5.3\"")).is_true();
        // Date should be today
        let today = Local::now().format("%Y-%m-%d").to_string();
        assert_that(&result.contains(&format!("date=\"{}\"", today))).is_true();
        // Other elements preserved
        assert_that(&result.contains("<component>")).is_true();
    }

    // -----------------------------------------------------------------------
    // delete_nelson_version_h
    // -----------------------------------------------------------------------

    #[test]
    fn delete_nelson_version_h_removes_existing_file() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("modules/commons/src/include");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("Nelson_VERSION.h");
        fs::write(&file, "// generated").unwrap();

        delete_nelson_version_h(tmp.path()).unwrap();
        assert_that(&file.exists()).is_false();
    }

    #[test]
    fn delete_nelson_version_h_noop_when_missing() {
        let tmp = TempDir::new().unwrap();
        // No file to delete – should not error
        let result = delete_nelson_version_h(tmp.path());
        assert_that(&result.is_ok()).is_true();
    }

    // -----------------------------------------------------------------------
    // edit_rc_files_in_modules
    // -----------------------------------------------------------------------

    #[test]
    fn edit_rc_files_in_modules_walks_and_updates() {
        let tmp = TempDir::new().unwrap();
        let mod_dir = tmp.path().join("modules/core/src");
        fs::create_dir_all(&mod_dir).unwrap();
        let rc_file = mod_dir.join("core.rc");
        fs::write(&rc_file, " FILEVERSION 0,0,0,0\n PRODUCTVERSION 0,0,0,0\n").unwrap();

        edit_rc_files_in_modules(tmp.path(), 3, 2, 1, 99).unwrap();

        let result = fs::read_to_string(&rc_file).unwrap();
        assert_that(&result.contains("FILEVERSION 3,2,1,99")).is_true();
        assert_that(&result.contains("PRODUCTVERSION 3,2,1,99")).is_true();
    }

    // -----------------------------------------------------------------------
    // GitHub CI helpers (pure logic tests)
    // -----------------------------------------------------------------------

    #[test]
    fn use_github_variables_returns_false_when_env_not_set() {
        // In a normal test environment, these should not be set
        // (unless running in CI, but even then we test the logic)
        std::env::remove_var("GITHUB_SHA");
        std::env::remove_var("GITHUB_RUN_NUMBER");
        assert_that(&use_github_variables()).is_false();
    }
}
