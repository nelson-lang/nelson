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
#define CAT_3_STRINGS(S1, S2, S3) S1 S2 S3
//=============================================================================
#if defined(_MSC_VER)
//=============================================================================
#if defined(_M_ARM64)
#define BOOST_TOOLSET "vc145"
#else
#define BOOST_TOOLSET "vc143"
#endif
//=============================================================================
#ifdef _DEBUG
/* multithreaded + debug */
#if defined(_M_ARM64)
/* no debug available on arm64 */
#define BOOST_RUNTIME "-mt"
#else
#define BOOST_RUNTIME "-mt-gd"
#endif
#else
/* multithreaded (release) */
#define BOOST_RUNTIME "-mt"
#endif
//=============================================================================
#if defined(_M_ARM64)
#define BOOST_ARCH "-a64-1_89"
#elif defined(_WIN64)
#define BOOST_ARCH "-x64-1_89"
#else
#define BOOST_ARCH "-x32-1_89"
#endif
//=============================================================================
#define BOOST_TARGET BOOST_TOOLSET BOOST_RUNTIME BOOST_ARCH
//=============================================================================
#else
/* Non-MSVC toolchains: BOOST_TARGET intentionally left undefined.
   If you need targets for other toolchains, add branches here. */
#endif
//=============================================================================
