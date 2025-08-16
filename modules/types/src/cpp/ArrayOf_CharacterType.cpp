//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Returns TRUE if we are a string.
 */
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
    return (dp->dataClass == NLS_CHAR) && (!dp->sparse) && !is2D();
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayToCharacterArray(const ArrayOf& stringArray, bool missingAsEmpty)
{
    if (!stringArray.isStringArray()) {
        Error(_W("String array expected."));
    }
    ArrayOf* ptr = (ArrayOf*)stringArray.getDataPointer();
    Dimensions dims = stringArray.getDimensions();
    wstringVector strs;
    strs.reserve(dims.getElementCount());
    indexType elementCount = dims.getElementCount();
    for (indexType k = 0; k < elementCount; ++k) {
        if (ptr[k].isCharacterArray()) {
            strs.push_back(ptr[k].getContentAsWideString());
        } else {
            if (missingAsEmpty) {
                strs.push_back(L"");
            } else {
                Error(_W("Conversion <missing> to character vector is not supported."));
            }
        }
    }
    return characterVectorToCharacterArray(strs, false);
}
//=============================================================================
ArrayOf
ArrayOf::characterVectorToCharacterArray(const stringVector& strs, bool leftAlign)
{
    ArrayOf res;
    if (strs.empty()) {
        res = ArrayOf::emptyConstructor();
        res.promoteType(NLS_CHAR);
    } else if (strs.size() == 1) {
        res = ArrayOf::characterArrayConstructor(strs[0]);
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
            NLS_CHAR, dims.getElementCount(), stringVector(), false);
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
            (charType*)ptr, dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
            (charType*)res.getDataPointer(), dims.getColumns(), dims.getRows());
        matTransposed = matOrigin.transpose().eval();
        Dimensions dimsOut(dims.getColumns(), dims.getRows());
        res.reshape(dimsOut);
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::characterVectorToCharacterArray(const wstringVector& strs, bool leftAlign)
{
    ArrayOf res;
    if (strs.empty()) {
        res = ArrayOf::emptyConstructor();
        res.promoteType(NLS_CHAR);
    } else if (strs.size() == 1) {
        res = ArrayOf::characterArrayConstructor(strs[0]);
    } else {
        size_t m = 0;
        for (const auto& s : strs) {
            if (m < s.size()) {
                m = s.size();
            }
        }
        Dimensions dims(m, strs.size());
        charType* ptr = (charType*)ArrayOf::allocateArrayOf(
            NLS_CHAR, dims.getElementCount(), stringVector(), false);
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
            (charType*)ptr, dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<charType, Eigen::Dynamic, Eigen::Dynamic>> matTransposed(
            (charType*)res.getDataPointer(), dims.getColumns(), dims.getRows());
        matTransposed = matOrigin.transpose().eval();
        Dimensions dimsOut(dims.getColumns(), dims.getRows());
        res.reshape(dimsOut);
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::characterArrayConstructor(const std::wstring& astr)
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
ArrayOf::characterArrayConstructor(const std::string& astr)
{
    return characterArrayConstructor(utf8_to_wstring(astr));
}
//=============================================================================
std::wstring
ArrayOf::getContentAsArrayOfCharacters() const
{
    std::wstring str;
    if (dp->dataClass == NLS_CHAR) {
        indexType M = getElementCount();
        auto* buffer = new_with_exception<charType>(M + 1, false);
        const auto* qp = static_cast<const charType*>(dp->getData());
        memcpy(buffer, qp, M * sizeof(charType));
        buffer[M] = 0;
        str = buffer;
        delete[] buffer;
    } else {
        Error(_W("Unable to convert supplied object to a string."));
    }
    return str;
}
//=============================================================================
char*
ArrayOf::getContentAsCharactersPointer() const
{
    std::string str = getContentAsCString();
    char* buffer = new_with_exception<char>(str.size() + 1, false);
    memcpy(buffer, str.c_str(), str.size() * sizeof(char));
    buffer[str.size()] = 0;
    return buffer;
}
//=============================================================================
wchar_t*
ArrayOf::getContentAsWideCharactersPointer() const
{
    charType* buffer = nullptr;
    if (isRowVectorCharacterArray()) {
        indexType M = getElementCount();
        buffer = new_with_exception<charType>(M + 1, false);
        const auto* qp = static_cast<const charType*>(dp->getData());
        memcpy(buffer, qp, M * sizeof(charType));
        buffer[M] = 0;
    } else {
        if (dp->dataClass != NLS_CHAR) {
            Error(_W("Unable to convert supplied object to a string."));
        }
        if (!isRowVector()) {
            Error(_W("Unable to convert supplied object to a single string."));
        }
    }
    return buffer;
}
//=============================================================================
std::string
ArrayOf::getContentAsCString() const
{
    if (isRowVectorCharacterArray()) {
        indexType M = getElementCount();
        const auto* qp = static_cast<const charType*>(dp->getData());
        // Preallocate buffer for worst-case UTF-8 size (4 bytes per char)
        size_t maxBytes = M * 4;
        std::unique_ptr<char[]> buffer(new char[maxBytes]);
        char* out = buffer.get();
        size_t outLen = 0;

        for (indexType i = 0; i < M; ++i) {
            char32_t wc = qp[i];
            if (wc <= 0x7F) {
                *out++ = static_cast<char>(wc);
                outLen += 1;
            } else if (wc <= 0x7FF) {
                *out++ = static_cast<char>(0xC0 | ((wc >> 6) & 0x1F));
                *out++ = static_cast<char>(0x80 | (wc & 0x3F));
                outLen += 2;
            } else if (wc <= 0xFFFF) {
                *out++ = static_cast<char>(0xE0 | ((wc >> 12) & 0x0F));
                *out++ = static_cast<char>(0x80 | ((wc >> 6) & 0x3F));
                *out++ = static_cast<char>(0x80 | (wc & 0x3F));
                outLen += 3;
            }
#if WCHAR_MAX > 0xFFFF
            else {
                *out++ = static_cast<char>(0xF0 | ((wc >> 18) & 0x07));
                *out++ = static_cast<char>(0x80 | ((wc >> 12) & 0x3F));
                *out++ = static_cast<char>(0x80 | ((wc >> 6) & 0x3F));
                *out++ = static_cast<char>(0x80 | (wc & 0x3F));
                outLen += 4;
            }
#endif
        }
        // Construct std::string from buffer and actual length
        return std::string(buffer.get(), outLen);
    } else {
        // Fallback: handle string arrays and error cases as before
        if (isStringArray()) {
            if (isScalar()) {
                auto* element = (ArrayOf*)getDataPointer();
                return element[0].getContentAsCString();
            }
            if (isEmpty()) {
                return {};
            }
        } else {
            if ((dp == nullptr) || dp->dataClass != NLS_CHAR) {
                Error(_W("Unable to convert supplied object to a string."));
            }
            if (!isRowVector()) {
                Error(_W("Unable to convert supplied object to a single string."));
            }
        }
        return {};
    }
}
//=============================================================================
std::wstring
ArrayOf::getContentAsWideString(size_t lengthMax) const
{
    std::wstring str;
    if (isRowVectorCharacterArray()) {
        indexType M = getElementCount();
        indexType N;
        if (lengthMax == std::string::npos || M <= lengthMax) {
            N = M;
        } else {
            N = lengthMax;
        }
        str.resize(N + 1);
        auto* buffer = new_with_exception<charType>(N + 1, false);
        const auto* qp = static_cast<const charType*>(dp->getData());
        memcpy(buffer, qp, N * sizeof(charType));
        buffer[N] = 0;
        str.assign(buffer);
        delete[] buffer;
    } else {
        if (isStringArray()) {
            if (isScalar()) {
                auto* element = (ArrayOf*)getDataPointer();
                return element[0].getContentAsWideString();
            }
            if (isEmpty()) {
                return {};
            }
        } else {
            if ((dp == nullptr) || dp->dataClass != NLS_CHAR) {
                Error(_W("Unable to convert supplied object to a string."));
            }
            if (!isRowVector()) {
                Error(_W("Unable to convert supplied object to a single string."));
            }
        }
    }
    return str;
}
//=============================================================================
stringVector
ArrayOf::getContentAsCStringVector(bool bCheckVector) const
{
    wstringVector wres = getContentAsWideStringVector(bCheckVector);
    stringVector res;
    res.reserve(wres.size());
    for (const auto& wre : wres) {
        res.push_back(wstring_to_utf8(wre));
    }
    return res;
}
//=============================================================================
wstringVector
ArrayOf::getContentAsWideStringVector(bool bCheckVector) const
{
    wstringVector res;
    if (dp->dataClass == NLS_CHAR) {
        if (is2D()) {
            indexType rows = getDimensions().getRows();
            indexType columns = getDimensions().getColumns();
            if (rows == 0 && columns == 0) {
                res.reserve(1);
                res.push_back(L"");
            } else {
                res.reserve(rows);
                for (indexType i = 0; i < rows; i++) {
                    std::wstring str;
                    str.reserve(columns);
                    const auto* qp = static_cast<const charType*>(dp->getData());
                    for (indexType j = 0; j < columns; j++) {
                        size_t idx = i + j * rows;
                        str.push_back(qp[idx]);
                    }
                    res.push_back(str);
                    str.clear();
                }
            }
        } else {
            res.push_back(getContentAsWideString());
        }
    } else {
        if (!isCell() && !isStringArray()) {
            Error(_W("A cell or string array expected."));
        } else if (isEmpty()) {
            return res;
        } else if (isVector() || !bCheckVector) {
            auto* arg = (ArrayOf*)(getDataPointer());
            indexType nbElements = getDimensions().getElementCount();
            res.reserve(nbElements);
            if (isCell()) {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].getDataClass() != NLS_CHAR) {
                        res.clear();
                        Error(_W("A cell of string expected."));
                    } else {
                        res.push_back(arg[k].getContentAsWideString());
                    }
                }
            } else {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].getDataClass() != NLS_CHAR) {
                        res.push_back(L"");
                    } else {
                        res.push_back(arg[k].getContentAsWideString());
                    }
                }
            }
        } else {
            Error(_W("A vector expected."));
        }
    }
    return res;
}
//=============================================================================
stringVector
ArrayOf::getContentAsCStringRowVector() const
{
    wstringVector wres = getContentAsWideStringRowVector();
    stringVector res;
    res.reserve(wres.size());
    for (const auto& wre : wres) {
        res.push_back(wstring_to_utf8(wre));
    }
    return res;
}
//=============================================================================
wstringVector
ArrayOf::getContentAsWideStringRowVector() const
{
    wstringVector res;
    if (!(isCell() || isStringArray())) {
        Error(_W("A cell or string array expected."));
    }
    if (isRowVector()) {
        auto* arg = (ArrayOf*)(getDataPointer());
        indexType nbElements = getDimensions().getElementCount();
        res.reserve(nbElements);
        if (isCell()) {
            for (indexType k = 0; k < nbElements; k++) {
                if (arg[k].getDataClass() != NLS_CHAR) {
                    res.clear();
                    Error(_W("A cell of string expected."));
                } else {
                    res.push_back(arg[k].getContentAsWideString());
                }
            }
        } else {
            for (indexType k = 0; k < nbElements; k++) {
                if (arg[k].getDataClass() != NLS_CHAR) {
                    res.push_back(L"");
                } else {
                    res.push_back(arg[k].getContentAsWideString());
                }
            }
        }
    } else {
        Error(_W("An row vector expected."));
    }
    return res;
}
//=============================================================================
stringVector
ArrayOf::getContentAsCStringColumnVector() const
{
    wstringVector wres = getContentAsWideStringColumnVector();
    stringVector res;
    res.reserve(wres.size());
    for (const auto& wre : wres) {
        res.push_back(wstring_to_utf8(wre));
    }
    return res;
}
//=============================================================================
wstringVector
ArrayOf::getContentAsWideStringColumnVector() const
{
    wstringVector res;
    if (!(isCell() || isStringArray())) {
        Error(_W("A cell or string array expected."));
    }
    if (!isEmpty()) {
        if (isColumnVector()) {
            auto* arg = (ArrayOf*)(getDataPointer());
            indexType nbElements = getDimensions().getElementCount();
            res.reserve(nbElements);
            if (isCell()) {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].getDataClass() != NLS_CHAR) {
                        res.clear();
                        Error(_W("A cell of string expected."));
                    } else {
                        res.push_back(arg[k].getContentAsWideString());
                    }
                }
            } else {
                for (indexType k = 0; k < nbElements; k++) {
                    if (arg[k].getDataClass() != NLS_CHAR) {
                        res.push_back(L"");
                    } else {
                        res.push_back(arg[k].getContentAsWideString());
                    }
                }
            }
        } else {
            Error(_W("A column vector expected."));
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
