//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>
//=============================================================================
#include "nlsTypes_exports.h"
#include "ClassdefLayout.hpp"
#include "Dimensions.hpp"
#include "Interface.hpp"
#include "Types.hpp"
#include "ArrayOfVector.hpp"
//=============================================================================
/**
 * C++ value API version exposed by ArrayOf.hpp.
 *
 * Use NELSON_ARRAYOF_API_VERSION_CHECK(major, minor, patch) in extension or
 * embedding code that depends on a specific ArrayOf C++ API revision.
 */
//=============================================================================
#define NELSON_ARRAYOF_API_VERSION_MAJOR 2
#define NELSON_ARRAYOF_API_VERSION_MINOR 0
#define NELSON_ARRAYOF_API_VERSION_PATCH 0
#define NELSON_ARRAYOF_API_VERSION_ENCODE(major, minor, patch)                                     \
    (((major) * 10000) + ((minor) * 100) + (patch))
#define NELSON_ARRAYOF_API_VERSION                                                                 \
    NELSON_ARRAYOF_API_VERSION_ENCODE(NELSON_ARRAYOF_API_VERSION_MAJOR,                            \
        NELSON_ARRAYOF_API_VERSION_MINOR, NELSON_ARRAYOF_API_VERSION_PATCH)
#define NELSON_ARRAYOF_API_VERSION_CHECK(major, minor, patch)                                      \
    (NELSON_ARRAYOF_API_VERSION >= NELSON_ARRAYOF_API_VERSION_ENCODE(major, minor, patch))
//=============================================================================
namespace Nelson {
/**
 * Wrap a scalar ArrayOf in an argument vector.
 */
NLSTYPES_IMPEXP ArrayOfVector
scalarArrayOfToArrayOfVector(const ArrayOf&);
class Data;
class HandleGenericObject;
/**
 * Runtime value container for Nelson arrays.
 *
 * ArrayOf stores the class tag, dimensions, field metadata, and payload
 * reference for scalar and N-dimensional values. Most payloads live in the
 * reference-counted Data block, which gives copies cheap value semantics and
 * lets writable access use copy-on-write. Real floating-point and logical scalars store their value
 * inline or own a transferred buffer without a Data block. Transferred buffers
 * keep their address while callers finish initializing them.
 *
 * Elements are addressed in column-major order. Reference classes such as
 * cells, structures, classes, function handles, and native handles store nested
 * values or handle states through the same Data mechanism.
 *
 * ArrayOf.hpp is the main entry point for the C++ value API. The
 * NELSON_ARRAYOF_API_VERSION macros expose the API revision for extension and
 * embedding code. The naming convention is: x() reads cheap properties, asX()
 * extracts existing C++ values, toX() creates new representations,
 * toAllocatedX() makes allocation explicit, setX() mutates, isX() tests, and
 * makeX() transforms in place.
 */
class NLSTYPES_IMPEXP ArrayOf
{
private:
    enum class Representation : uint8_t
    {
        DataRef,
        InlineScalar,
        OwnedScalar
    };

    std::string _name;
    /**
     * Shared storage block used when the value does not use compact scalar storage.
     */
    Data* dp = nullptr;
    Representation representation_ = Representation::DataRef;
    uint8_t scalarDimensionCount_ = 2;
    uint8_t scalarType_ = NLS_DOUBLE;
    union ScalarStorage
    {
        double doubleValue = 0.;
        single singleValue;
        logical logicalValue;
        void* owned;
    } scalarStorage_;
#include "ArrayOfPrivateDeclarations.hpp"

public:
#include "ArrayOfCoreDeclarations.hpp"
#include "ArrayOfConstructorsDeclarations.hpp"
#include "ArrayOfIndexingDeclarations.hpp"
#include "ArrayOfContentDeclarations.hpp"
#include "ArrayOfStructDeclarations.hpp"
#include "ArrayOfClassDeclarations.hpp"
#include "ArrayOfHandleDeclarations.hpp"
#include "ArrayOfStringDeclarations.hpp"
#include "ArrayOfTableDeclarations.hpp"
#include "ArrayOfMissingDeclarations.hpp"
};
//=============================================================================
/**
 * Return whether a value represents the colon operator token.
 */
bool
isColonOperator(const ArrayOf& a);
//=============================================================================
} // namespace Nelson
//=============================================================================
