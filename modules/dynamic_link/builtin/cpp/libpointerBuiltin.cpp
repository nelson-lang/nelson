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
#include "libpointerBuiltin.hpp"
#include "Error.hpp"
#include "LibPointerObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::libpointerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    LibPointerObject* libPointerObject = nullptr;
    std::wstring DataType;
    ArrayOf Value;
    switch (argIn.size()) {
    case 0: {
        try {
            libPointerObject = new LibPointerObject();
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        } catch (const Exception&) {
            throw;
        }
    } break;
    case 1: {
        std::wstring DataType = argIn[0].getContentAsWideString();
        try {
            libPointerObject = new LibPointerObject(DataType);
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        } catch (const Exception&) {
            throw;
        }
    } break;
    case 2: {
        std::wstring DataType = argIn[0].getContentAsWideString();
        ArrayOf Value = argIn[1];
        try {
            libPointerObject = new LibPointerObject(DataType, Value);
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        } catch (const Exception&) {
            throw;
        }
    } break;
    default:
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        break;
    }
    retval.push_back(ArrayOf::handleConstructor(libPointerObject));
    return retval;
}
//=============================================================================
