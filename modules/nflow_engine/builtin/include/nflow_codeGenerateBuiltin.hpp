//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
ArrayOfVector
nflow_codeGenerateBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// [cSupported, rustSupported] = __nflow_codegen_support__(blockType)
// Reports, for a block type, whether the C and Rust generators can lower it
// (a codegen template is registered and not marked notImplemented). The
// authoritative source for the per-block "supports code generation" help
// indicator.
ArrayOfVector
nflow_codegenSupportBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// types = __nflow_codegen_asymmetries__()
// Cell array of block types registered for exactly ONE codegen target (C
// without Rust or vice versa). Always empty when the registries are healthy;
// the test suite asserts emptiness so a one-sided registration cannot land
// silently.
ArrayOfVector
nflow_codegenAsymmetriesBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// tf = __nflow_codegen_toolchain__('C' | 'rust')
// True when the build toolchain for the language is available (cmake + a C
// compiler, resp. cargo + rustc). Probes are process-cached, so repeated
// calls are free; tests use this as their skip guard (the MEX-oriented
// <--C/C++ COMPILER REQUIRED--> tag probes a different toolchain).
ArrayOfVector
nflow_codegenToolchainBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
