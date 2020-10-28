//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
        return std::string();
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
        } catch (boost::archive::archive_exception&) {
            success = false;
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
