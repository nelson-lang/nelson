//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once

#ifdef __INTEL_COMPILER
#if defined(_OPENMP)
#define __NLS_WITH_OPENMP
#endif
#endif

#if (defined(_LP64) || defined(_WIN64))
#define NLS_INDEX_TYPE_64
#endif

#define CAT_3_STRINGS(a, b, c) a##b##c
#ifdef _MSC_VER
#ifdef _DEBUG
#ifdef _WIN64
#define BOOST_TARGET "vc141-mt-gd-x64-1_67"
#else
#define BOOST_TARGET "vc141-mt-gd-x32-1_67"
#endif
#else
#ifdef _WIN64
#define BOOST_TARGET "vc141-mt-x64-1_67"
#else
#define BOOST_TARGET "vc141-mt-x32-1_67"
#endif
#endif
#endif