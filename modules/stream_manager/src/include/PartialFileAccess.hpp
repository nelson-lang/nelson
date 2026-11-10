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
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// a strided block: one (start, stride, count) per subscript, 1-based start
struct NLSSTREAM_MANAGER_IMPEXP PartialFileBlock
{
    std::vector<indexType> start;
    std::vector<indexType> stride;
    std::vector<indexType> count;
    indexType
    last(size_t k) const;
    indexType
    elementCount() const;
    // dimensions of the selected block, trailing singletons removed
    Dimensions
    resultDimensions() const;
};
//=============================================================================
// what a file says about one variable, read without its data
struct PartialFileVariable
{
    bool fileExists = false;
    bool exists = false;
    // class of the variable, NLS_UNKNOWN for objects and other classes
    NelsonType type = NLS_UNKNOWN;
    bool isSparse = false;
    Dimensions dims;
};
//=============================================================================
// file format side of matfile and nh5file
class NLSSTREAM_MANAGER_IMPEXP PartialFileBackend
{
public:
    virtual ~PartialFileBackend() = default;
    virtual std::string
    fileText() const = 0;
    virtual PartialFileVariable
    describe(const std::string& name) = 0;
    virtual ArrayOf
    readWhole(const std::string& name) = 0;
    virtual void
    writeWhole(const std::string& name, const ArrayOf& value) = 0;
    // block access in place; false makes the caller load the whole variable
    virtual bool
    readBlock(const std::string& name, const PartialFileVariable& variable,
        const PartialFileBlock& block, ArrayOf& result) = 0;
    virtual bool
    writeBlock(const std::string& name, const PartialFileVariable& variable,
        const PartialFileBlock& block, const ArrayOf& data) = 0;
    // called before a block access falls back to the whole variable
    virtual void
    beforeWholeAccess(const std::string& name);
};
//=============================================================================
// the subscripts cell of obj.name(i1, ..., iN) as a list
NLSSTREAM_MANAGER_IMPEXP ArrayOfVector
PartialFileSubscripts(const ArrayOf& subscripts);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP ArrayOf
PartialFileVariableSize(PartialFileBackend& backend, const std::string& name);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP ArrayOf
PartialFileRead(PartialFileBackend& backend, const std::string& name, const ArrayOfVector* indices);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP void
PartialFileWrite(PartialFileBackend& backend, const std::string& name, const ArrayOfVector* indices,
    const ArrayOf& value);
//=============================================================================
} // namespace Nelson
//=============================================================================
