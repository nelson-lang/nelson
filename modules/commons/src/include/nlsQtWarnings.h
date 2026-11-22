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
// Qt 6.11 <QtCore/qarraydata.h> places [[nodiscard]] where MSVC (VS2026)
// reports C5240 "attribute is ignored in this syntactic position". The attribute
// is harmless and the warning is a Qt/compiler-version artifact, so it is silenced
// for the Qt-based modules only.
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 5240)
#endif
//=============================================================================
