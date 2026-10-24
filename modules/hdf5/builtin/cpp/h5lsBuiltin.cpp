//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5lsBuiltin.hpp"
#include "h5LsFile.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
entriesToCell(const std::vector<H5LsEntry>& entries)
{
    Dimensions dims(entries.size(), 2);
    if (entries.empty()) {
        return ArrayOf::emptyCell(dims);
    }
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false));
    for (indexType k = 0; k < static_cast<indexType>(entries.size()); ++k) {
        elements[k] = ArrayOf::characterArray(entries[k].path);
        elements[k + static_cast<indexType>(entries.size())]
            = ArrayOf::characterArray(entries[k].type);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
ArrayOfVector
Hdf5Gateway::h5lsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    std::wstring filename = argIn[0].asWideString();
    std::wstring location;
    if (argIn.size() == 2) {
        location = argIn[1].asWideString();
    }
    std::vector<H5LsEntry> entries = h5LsFile(filename, location);
    if (nLhs == 1) {
        retval << entriesToCell(entries);
    } else {
        Interface* io = eval->getInterface();
        for (const H5LsEntry& entry : entries) {
            io->outputMessage(entry.path + " " + entry.type + "\n");
        }
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
