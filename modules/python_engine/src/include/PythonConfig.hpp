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
#ifdef _MSC_VER
#define MS_NO_COREDLL
#endif
#define Py_CPYTHON_COMPLEXOBJECT_H
#include <Python.h>
#undef PyBool_Type
#undef PyComplex_Type
#undef PyFloat_Type
#undef PyNone
//=============================================================================
