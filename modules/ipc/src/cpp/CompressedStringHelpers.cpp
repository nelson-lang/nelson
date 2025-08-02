//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include "CompressedStringHelpers.hpp"
#include "ArrayOfSerialization.hpp"
#include "StringZLib.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
ArrayOfToCompressedString(const ArrayOf& data, bool& fullySerialized)
{
    ArrayOfSerialization serializedVariable(data);
    fullySerialized = serializedVariable.isFullySerialized();
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << serializedVariable;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        fullySerialized = false;
        return {};
    }
    return serialized_compressed_string;
}
//=============================================================================
ArrayOf
CompressedStringToArrayOf(const std::string& compressedString, bool& success)
{
    ArrayOf res;
    success = false;
    bool failed = false;
    std::string decompressedVariable = decompressString(compressedString, failed);
    if (!failed) {
        std::stringstream iss;
        iss << decompressedVariable;
        decompressedVariable.clear();
        ArrayOfSerialization serializedVariable;
        try {
            boost::archive::binary_iarchive ia(iss);
            ia >> serializedVariable;
            res = serializedVariable.get(success);
            if (!success && !serializedVariable.isFullySerialized()) {
                success = true;
            }
        } catch (std::exception&) {
            success = false;
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
