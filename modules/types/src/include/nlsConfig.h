//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if defined(_MSC_VER)
#define EIGEN_USE_MKL
#define EIGEN_USE_MKL_VML
#define _NLS_WITH_VML
#endif
//=============================================================================
#if defined(_OPENMP)
#define _NLS_WITH_OPENMP
#endif
//=============================================================================
#if (defined(_LP64) || defined(_WIN64))
#define NLS_INDEX_TYPE_64
#endif
//=============================================================================
#define CAT_3_STRINGS(a, b, c) a##b##c
#ifdef _MSC_VER
#ifdef _DEBUG
#ifdef _WIN64
#define BOOST_TARGET "vc143-mt-gd-x64-1_78"
#else
#define BOOST_TARGET "vc143-mt-gd-x32-1_78"
#endif
#else
#ifdef _WIN64
#define BOOST_TARGET "vc143-mt-x64-1_78"
#else
#define BOOST_TARGET "vc143-mt-x32-1_78"
#endif
#endif
#endif
//=============================================================================
