// ==============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ==============================================================================
// This file is part of Nelson.
// =============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ==============================================================================

fn main() {
    // Embed a Windows application manifest that explicitly sets
    // requestedExecutionLevel to "asInvoker". Without this, Windows UAC
    // heuristics flag any executable with "update" in its name for elevation
    // (OS error 740), even when no admin rights are needed.
    #[cfg(target_os = "windows")]
    embed_manifest::embed_manifest(embed_manifest::new_manifest("nelson-update-version"))
        .expect("unable to embed manifest");
}
