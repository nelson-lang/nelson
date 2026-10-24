// ==============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ==============================================================================
// This file is part of Nelson.
// ==============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ==============================================================================

use std::process::{Command, ExitCode};

const TYPOS_CLI_VERSION: &str = "1.48.0";

fn main() -> ExitCode {
    let args = std::env::args_os().skip(1).collect::<Vec<_>>();

    match run_typos(&args) {
        Ok(code) => code,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            match install_typos() {
                Ok(code) if code == ExitCode::SUCCESS => {}
                Ok(code) => return code,
                Err(install_error) => {
                    eprintln!("Failed to install typos-cli: {install_error}");
                    return ExitCode::FAILURE;
                }
            }
            run_typos(&args).unwrap_or_else(|retry_error| {
                eprintln!("Failed to run typos after install: {retry_error}");
                ExitCode::FAILURE
            })
        }
        Err(error) => {
            eprintln!("Failed to run typos: {error}");
            ExitCode::FAILURE
        }
    }
}

fn run_typos(args: &[std::ffi::OsString]) -> std::io::Result<ExitCode> {
    let status = Command::new("typos").args(args).status()?;
    Ok(status_to_exit_code(status))
}

fn install_typos() -> std::io::Result<ExitCode> {
    let status = Command::new("cargo")
        .args([
            "install",
            "typos-cli",
            "--locked",
            "--version",
            TYPOS_CLI_VERSION,
        ])
        .status()?;
    Ok(status_to_exit_code(status))
}

fn status_to_exit_code(status: std::process::ExitStatus) -> ExitCode {
    match status.code() {
        Some(0) => ExitCode::SUCCESS,
        Some(code) => ExitCode::from(code.clamp(1, 255) as u8),
        None => ExitCode::FAILURE,
    }
}
