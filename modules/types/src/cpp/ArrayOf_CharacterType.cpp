//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isCharacterArray() const
{
    if (dp) {
        return (dp->dataClass == NLS_CHAR) && (!dp->sparse);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isRowVectorCharacterArray() const
{
    return (isCharacterArray() && (!dp->sparse) && (isRowVector() || isEmpty()));
}
//=============================================================================
bool
ArrayOf::isNdArrayCharacterType() const
{
    return (dataClass() == NLS_CHAR) && !isSparse() && !is2D();
}
//=============================================================================
ArrayOf
ArrayOf::characterArrayFromStringArray(const ArrayOf& stringArray, bool missingAsEmpty)
{
    if (!stringArray.isStringArray()) {
        Error(L"Nelson:types:stringArrayExpected", L"String array expected.");
    }
    ArrayOf* ptr = (ArrayOf*)stringArray.dataPointer();
    Dimensions dims = stringArray.dimensions();
    wstringVector strs;
    strs.reserve(dims.elementCount());
    indexType elementCount = dims.elementCount();
    for (indexType k = 0; k < elementCount; ++k) {
        if (ptr[k].isCharacterArray()) {
            strs.push_back(ptr[k].asWideString());
        } else {
            if (missingAsEmpty) {
                strs.push_back(L"");
            } else {
                Error(L"Nelson:types:conversionNotSupported:"
                      L"conversionMissingCharacterVectorNotSupported",
                    L"Conversion <missing> to character vector is not supported.");
            }
        }
    }
    return characterArrayFromRows(strs, false);
}
//=============================================================================
ArrayOf
ArrayOf::characterArrayFromRows(const stringVector& strs, bool leftAlign)
{
    ArrayOf res;
    if (strs.empty()) {
        res = ArrayOf::empty();
        res.promoteType(NLS_CHAR);
    } else if (strs.size() == 1) {
        res = ArrayOf::characterArray(strs[0]);
    } else {
        size_t m = 0;
        wstringVector sw;
        sw.reserve(strs.size());
        for (const std::string& s : strs) {
            std::wstring u = utf8_to_wstring(s);
            sw.push_back(u);
            if (m < u.size()) {
                m = u.size();
            }
        }
        Dimensions dims(m, sw.size());
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(
            NLS_CHAR, dims.elementCount(), stringVector(), false);
        res = ArrayOf(NLS_CHAR, dims, ptr);
        std::wstring blanks;
        blanks.reserve(m);
        size_t k = 0;
        for (const auto& s : sw) {
            size_t nbBlanks = m - s.size();
            if (leftAlign) {
                if (nbBlanks > 0) {
                    blanks = std::wstring(nbBlanks, L' ');
                    memcpy(&ptr[k], blanks.c_str(), sizeof(charType) * nbBlanks);
                }
                memcpy(&ptr[k + nbBlanks], s.c_str(), sizeof(charType) * s.size());
            } else {
                memcpy(&ptr[k], s.c_str(), sizeof(charType) * s.size());
                if (nbBlanks > 0) {
                    blanks = std::wstring(nbBlanks, L' ');
                    memcpy(&ptr[k + s.size()], blanks.c_str(), sizeof(charType) * nbBlanks);
                }
            }
            k = k + m;
        }
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (charType*)ptr, dims.rows(), dims.columns());
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
            (charType*)res.dataPointer(), dims.columns(), dims.rows());
        matTransposed = matOrigin.transpose().eval();
        Dimensions dimsOut(dims.columns(), dims.rows());
        res.reshape(dimsOut);
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::characterArrayFromRows(const wstringVector& strs, bool leftAlign)
{
    ArrayOf res;
    if (strs.empty()) {
        res = ArrayOf::empty();
        res.promoteType(NLS_CHAR);
    } else if (strs.size() == 1) {
        res = ArrayOf::characterArray(strs[0]);
    } else {
        size_t m = 0;
        for (const auto& s : strs) {
            if (m < s.size()) {
                m = s.size();
            }
        }
        Dimensions dims(m, strs.size());
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(
            NLS_CHAR, dims.elementCount(), stringVector(), false);
        res = ArrayOf(NLS_CHAR, dims, ptr);
        std::wstring blanks;
        blanks.reserve(m);
        size_t k = 0;
        for (const auto& s : strs) {
            size_t nbBlanks = m - s.size();
            if (leftAlign) {
                if (nbBlanks > 0) {
                    blanks = std::wstring(nbBlanks, L' ');
                    memcpy(&ptr[k], blanks.c_str(), sizeof(charType) * nbBlanks);
                }
                memcpy(&ptr[k + nbBlanks], s.c_str(), sizeof(charType) * s.size());
            } else {
                memcpy(&ptr[k], s.c_str(), sizeof(charType) * s.size());
                if (nbBlanks > 0) {
                    blanks = std::wstring(nbBlanks, L' ');
                    memcpy(&ptr[k + s.size()], blanks.c_str(), sizeof(charType) * nbBlanks);
                }
            }
            k = k + m;
        }
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (charType*)ptr, dims.rows(), dims.columns());
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
            (charType*)res.dataPointer(), dims.columns(), dims.rows());
        matTransposed = matOrigin.transpose().eval();
        Dimensions dimsOut(dims.columns(), dims.rows());
        res.reshape(dimsOut);
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::characterArray(const std::wstring& astr)
{
    indexType length = astr.length();
    Dimensions dim(0, length);
    if (length == 0) {
        dim[0] = 0;
    } else {
        dim[0] = 1;
    }
    charType* cp = static_cast<charType*>(allocateArrayOf(NLS_CHAR, length, stringVector(), false));
    memcpy(cp, astr.c_str(), length * sizeof(charType));
    return ArrayOf(NLS_CHAR, dim, cp);
}
//=============================================================================
ArrayOf
ArrayOf::characterArray(const std::string& astr)
{
    return characterArray(utf8_to_wstring(astr));
}
//=============================================================================
std::wstring
ArrayOf::asCharacterArrayWideString() const
{
    std::wstring str;
    if (dp->dataClass == NLS_CHAR) {
        indexType M = elementCount();
        auto* buffer = new_with_exception<charType>(M + 1, false);
        const auto* qp = static_cast<const charType*>(dp->data());
        memcpy(buffer, qp, M * sizeof(charType));
        buffer[M] = 0;
        str = buffer;
        delete[] buffer;
    } else {
        Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
    }
    return str;
}
//=============================================================================
char*
ArrayOf::toAllocatedUtf8CString() const
{
    std::string str = asUtf8String();
    char* buffer = new_with_exception<char>(str.size() + 1, false);
    memcpy(buffer, str.c_str(), str.size() * sizeof(char));
    buffer[str.size()] = 0;
    return buffer;
}
//=============================================================================
wchar_t*
ArrayOf::toAllocatedWideCString() const
{
    charType* buffer = nullptr;
    if (isRowVectorCharacterArray()) {
        indexType M = elementCount();
        buffer = new_with_exception<charType>(M + 1, false);
        const auto* qp = static_cast<const charType*>(dp->data());
        memcpy(buffer, qp, M * sizeof(charType));
        buffer[M] = 0;
    } else {
        if (dp->dataClass != NLS_CHAR) {
            Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
        }
        if (!isRowVector()) {
            Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
        }
    }
    return buffer;
}
//=============================================================================
std::string
ArrayOf::asUtf8String() const
{
    if (isRowVectorCharacterArray()) {
        indexType M = elementCount();
        const auto* qp = static_cast<const charType*>(dp->data());
        return wstring_to_utf8(std::wstring(qp, qp + M));
    } else {
        if (isStringArray()) {
            if (isScalar()) {
                auto* element = (ArrayOf*)dataPointer();
                return element[0].asUtf8String();
            }
            if (isEmpty()) {
                return {};
            }
        } else {
            if ((dp == nullptr) || dp->dataClass != NLS_CHAR) {
                Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
            }
            if (!isRowVector()) {
                Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
            }
        }
        return {};
    }
}
//=============================================================================
std::wstring
ArrayOf::asWideString(size_t lengthMax) const
{
    std::wstring str;
    if (isRowVectorCharacterArray()) {
        indexType M = elementCount();
        indexType N;
        if (lengthMax == std::string::npos || M <= lengthMax) {
            N = M;
        } else {
            N = lengthMax;
        }
        str.resize(N + 1);
        auto* buffer = new_with_exception<charType>(N + 1, false);
        const auto* qp = static_cast<const charType*>(dp->data());
        memcpy(buffer, qp, N * sizeof(charType));
        buffer[N] = 0;
        str.assign(buffer);
        delete[] buffer;
    } else {
        if (isStringArray()) {
            if (isScalar()) {
                auto* element = (ArrayOf*)dataPointer();
                return element[0].asWideString();
            }
            if (isEmpty()) {
                return {};
            }
        } else {
            if ((dp == nullptr) || dp->dataClass != NLS_CHAR) {
                Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
            }
            if (!isRowVector()) {
                Error(L"Nelson:stringExpected", L"Unable to convert supplied object to a string.");
            }
        }
    }
    return str;
}
//=============================================================================
stringVector
ArrayOf::asUtf8StringVector(bool bCheckVector) const
{
    wstringVector wres = asWideStringVector(bCheckVector);
    stringVector res;
    res.reserve(wres.size());
    for (const auto& wre : wres) {
        res.push_back(wstring_to_utf8(wre));
    }
    return res;
}
//=============================================================================
wstringVector
ArrayOf::asWideStringVector(bool bCheckVector) const
{
    wstringVector res;
    if (dp->dataClass == NLS_CHAR) {
        if (is2D()) {
            indexType rows = dimensions().rows();
            indexType columns = dimensions().columns();
            if (rows == 0 && columns == 0) {
                res.reserve(1);
                res.push_back(L"");
            } else {
                res.reserve(rows);
                for (indexType i = 0; i < rows; i++) {
                    std::wstring str;
                    str.reserve(columns);
                    const auto* qp = static_cast<const charType*>(dp->data());
                    for (indexType j = 0; j < columns; j++) {
                        size_t idx = i + j * rows;
                        str.push_back(qp[idx]);
                    }
                    res.push_back(str);
                    str.clear();
                }
            }
        } else {
            res.push_back(asWideString());
        }
    } else {
        if (!isCell() && !isStringArray()) {
            Error(L"Nelson:types:cellContentsExpected:aCellStringArrayExpected",
                L"A cell or string array expected.");
        } else if (isEmpty()) {
            return res;
        } else if (isVector() || !bCheckVector) {
            auto* arg = (ArrayOf*)(dataPointer());
            indexType nbElements = dimensions().elementCount();
            res.reserve(nbElements);
            if (isCell()) {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].dataClass() != NLS_CHAR) {
                        res.clear();
                        Error(L"Nelson:types:cellContentsExpected", L"A cell of string expected.");
                    } else {
                        res.push_back(arg[k].asWideString());
                    }
                }
            } else {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].dataClass() != NLS_CHAR) {
                        res.push_back(L"");
                    } else {
                        res.push_back(arg[k].asWideString());
                    }
                }
            }
        } else {
            Error(L"Nelson:expectedVector", L"%s must be a vector.", L"Value");
        }
    }
    return res;
}
//=============================================================================
stringVector
ArrayOf::asUtf8StringRowVector() const
{
    wstringVector wres = asWideStringRowVector();
    stringVector res;
    res.reserve(wres.size());
    for (const auto& wre : wres) {
        res.push_back(wstring_to_utf8(wre));
    }
    return res;
}
//=============================================================================
wstringVector
ArrayOf::asWideStringRowVector() const
{
    wstringVector res;
    if (!(isCell() || isStringArray())) {
        Error(L"Nelson:types:cellContentsExpected:aCellStringArrayExpected",
            L"A cell or string array expected.");
    }
    if (isRowVector()) {
        auto* arg = (ArrayOf*)(dataPointer());
        indexType nbElements = dimensions().elementCount();
        res.reserve(nbElements);
        if (isCell()) {
            for (indexType k = 0; k < nbElements; k++) {
                if (arg[k].dataClass() != NLS_CHAR) {
                    res.clear();
                    Error(L"Nelson:types:cellContentsExpected", L"A cell of string expected.");
                } else {
                    res.push_back(arg[k].asWideString());
                }
            }
        } else {
            for (indexType k = 0; k < nbElements; k++) {
                if (arg[k].dataClass() != NLS_CHAR) {
                    res.push_back(L"");
                } else {
                    res.push_back(arg[k].asWideString());
                }
            }
        }
    } else {
        Error(L"Nelson:validators:mustBeRow", L"%s must be a row vector.", L"Value");
    }
    return res;
}
//=============================================================================
stringVector
ArrayOf::asUtf8StringColumnVector() const
{
    wstringVector wres = asWideStringColumnVector();
    stringVector res;
    res.reserve(wres.size());
    for (const auto& wre : wres) {
        res.push_back(wstring_to_utf8(wre));
    }
    return res;
}
//=============================================================================
wstringVector
ArrayOf::asWideStringColumnVector() const
{
    wstringVector res;
    if (!(isCell() || isStringArray())) {
        Error(L"Nelson:types:cellContentsExpected:aCellStringArrayExpected",
            L"A cell or string array expected.");
    }
    if (!isEmpty()) {
        if (isColumnVector()) {
            auto* arg = (ArrayOf*)(dataPointer());
            indexType nbElements = dimensions().elementCount();
            res.reserve(nbElements);
            if (isCell()) {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].dataClass() != NLS_CHAR) {
                        res.clear();
                        Error(L"Nelson:types:cellContentsExpected", L"A cell of string expected.");
                    } else {
                        res.push_back(arg[k].asWideString());
                    }
                }
            } else {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].dataClass() != NLS_CHAR) {
                        res.push_back(L"");
                    } else {
                        res.push_back(arg[k].asWideString());
                    }
                }
            }
        } else {
            Error(L"Nelson:validators:mustBeColumn", L"%s must be a column vector.", L"Value");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
