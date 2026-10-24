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
// Calling convention of the SLICOT routines linked into Nelson.
//
// Native builds link the Fortran library and keep the historical declarations.
// WebAssembly links the f2c-converted C sources (tools/wasm/build-slicot-f2c.sh):
// every subroutine returns void and takes one trailing int length argument per
// CHARACTER argument. WebAssembly validates each call signature, so a
// declaration that does not match traps with "unreachable" at the call.
//=============================================================================
#ifdef __EMSCRIPTEN__
#define SLICOT_SUBROUTINE void
#define SLICOT_STRLEN_T , int
#define SLICOT_STRLEN_V , 1
#else
#define SLICOT_SUBROUTINE int
#define SLICOT_STRLEN_T
#define SLICOT_STRLEN_V
#endif
//=============================================================================
// SLICOT_STRLEN_DECL(n) / SLICOT_STRLEN_ARGS(n): the hidden length parameters
// and arguments of a routine with n CHARACTER arguments.
#define SLICOT_STRLEN_DECL(n) SLICOT_STRLEN_DECL_##n
#define SLICOT_STRLEN_ARGS(n) SLICOT_STRLEN_ARGS_##n
#define SLICOT_STRLEN_DECL_0
#define SLICOT_STRLEN_DECL_1 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_2 SLICOT_STRLEN_DECL_1 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_3 SLICOT_STRLEN_DECL_2 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_4 SLICOT_STRLEN_DECL_3 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_5 SLICOT_STRLEN_DECL_4 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_6 SLICOT_STRLEN_DECL_5 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_7 SLICOT_STRLEN_DECL_6 SLICOT_STRLEN_T
#define SLICOT_STRLEN_DECL_8 SLICOT_STRLEN_DECL_7 SLICOT_STRLEN_T
#define SLICOT_STRLEN_ARGS_0
#define SLICOT_STRLEN_ARGS_1 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_2 SLICOT_STRLEN_ARGS_1 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_3 SLICOT_STRLEN_ARGS_2 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_4 SLICOT_STRLEN_ARGS_3 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_5 SLICOT_STRLEN_ARGS_4 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_6 SLICOT_STRLEN_ARGS_5 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_7 SLICOT_STRLEN_ARGS_6 SLICOT_STRLEN_V
#define SLICOT_STRLEN_ARGS_8 SLICOT_STRLEN_ARGS_7 SLICOT_STRLEN_V
//=============================================================================
