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
namespace Nelson {
//=============================================================================
// Canonical names of the built-in nelson.* classes (mixins and indexing helpers).
inline constexpr const char* NLS_INDEXING_OPERATION = "nelson.indexing.IndexingOperation";
inline constexpr const char* NLS_INDEXING_OPERATION_TYPE = "nelson.indexing.IndexingOperationType";
inline constexpr const char* NLS_METADATA_CLASS = "nelson.metadata.Class";
inline constexpr const char* NLS_METADATA_PROPERTY = "nelson.metadata.Property";
inline constexpr const char* NLS_METADATA_METHOD = "nelson.metadata.Method";
inline constexpr const char* NLS_METADATA_EVENT = "nelson.metadata.Event";
inline constexpr const char* NLS_METADATA_ENUMERATED_VALUE = "nelson.metadata.EnumeratedValue";
inline constexpr const char* NLS_META_CLASS = "meta.class";
inline constexpr const char* NLS_META_PROPERTY = "meta.property";
inline constexpr const char* NLS_META_METHOD = "meta.method";
inline constexpr const char* NLS_META_EVENT = "meta.event";
inline constexpr const char* NLS_META_ENUMERATION_MEMBER = "meta.EnumerationMember";
inline constexpr const char* NLS_MIXIN_COPYABLE = "nelson.mixin.Copyable";
inline constexpr const char* NLS_MIXIN_CUSTOM_DISPLAY = "nelson.mixin.CustomDisplay";
inline constexpr const char* NLS_MIXIN_SET_GET = "nelson.mixin.SetGet";
inline constexpr const char* NLS_MIXIN_SET_GET_EXACT_NAMES = "nelson.mixin.SetGetExactNames";
inline constexpr const char* NLS_MIXIN_HETEROGENEOUS = "nelson.mixin.Heterogeneous";
inline constexpr const char* NLS_MIXIN_SCALAR = "nelson.mixin.Scalar";
inline constexpr const char* NLS_MIXIN_CUSTOM_COMPACT_DISPLAY_PROVIDER
    = "nelson.mixin.CustomCompactDisplayProvider";
inline constexpr const char* NLS_MIXIN_INDEXING_REDEFINES_PAREN
    = "nelson.mixin.indexing.RedefinesParen";
inline constexpr const char* NLS_MIXIN_INDEXING_REDEFINES_BRACE
    = "nelson.mixin.indexing.RedefinesBrace";
inline constexpr const char* NLS_MIXIN_INDEXING_REDEFINES_DOT
    = "nelson.mixin.indexing.RedefinesDot";
//=============================================================================
} // namespace Nelson
//=============================================================================
