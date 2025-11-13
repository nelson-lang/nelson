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
#ifdef _MSC_VER
#ifdef _DEBUG
#ifdef _WIN64
#define BOOST_TARGET "vc143-mt-gd-x64-1_89"
#else
#define BOOST_TARGET "vc143-mt-gd-x32-1_89"
#endif
#else
#ifdef _WIN64
#define BOOST_TARGET "vc143-mt-x64-1_89"
#else
#define BOOST_TARGET "vc143-mt-x32-1_89"
#endif
#endif
#endif
//=============================================================================
