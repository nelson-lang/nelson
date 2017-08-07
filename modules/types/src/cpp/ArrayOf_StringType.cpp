//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "characters_encoding.hpp"
#include "ArrayOf.hpp"
#include "Data.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    /**
    * Returns TRUE if we are a string.
    */
    const bool ArrayOf::isString() const
    {
        return (dp->dataClass == NLS_CHAR) && (!dp->sparse);
    }
    //=============================================================================
    const bool ArrayOf::isSingleString() const
    {
        return (isString() && (!dp->sparse) && (isRowVector() || isEmpty()));
    }
    //=============================================================================
    const bool ArrayOf::isNdArrayStringType() const
    {
        return (dp->dataClass == NLS_CHAR) && (!dp->sparse) && !is2D();
    }
    //=============================================================================
    ArrayOf ArrayOf::stringConstructor(std::wstring astr)
    {
        charType *cp = nullptr;
        indexType length = astr.length();
        Dimensions dim(0, length);
        if (length == 0)
        {
            dim[0] = 0;
        }
        else
        {
            dim[0] = 1;
        }
        cp = (charType *)allocateArrayOf(NLS_CHAR, length);
        memcpy(cp, astr.c_str(), length * sizeof(charType));
        return ArrayOf(NLS_CHAR, dim, cp);
    }
    //=============================================================================
    ArrayOf ArrayOf::stringConstructor(std::string astr)
    {
        std::wstring str = utf8_to_wstring(astr);
        return stringConstructor(str);
    }
    //=============================================================================
    std::string ArrayOf::getContentAsCString(void) const
    {
        return wstring_to_utf8(getContentAsWideString());
    }
    //=============================================================================
    std::wstring ArrayOf::getContentAsArrayOfCharacters() const
    {
        std::wstring str = L"";
        if (dp->dataClass == NLS_CHAR)
        {
            charType *buffer = nullptr;
            const charType *qp = nullptr;
            indexType M = getLength();
            buffer = new_with_exception<charType>(M + 1);
            qp = (const charType*)dp->getData();
            memcpy(buffer, qp, M * sizeof(charType));
            buffer[M] = 0;
            str = buffer;
            delete[] buffer;
        }
        else
        {
            throw Exception(_W("Unable to convert supplied object to a string."));
        }
        return str;
    }
    //=============================================================================
    std::wstring ArrayOf::getContentAsWideString(void) const
    {
        std::wstring str = L"";
        if (isSingleString())
        {
            charType *buffer = nullptr;
            const charType *qp = nullptr;
            indexType M = getLength();
            buffer = new_with_exception<charType>(M + 1);
            qp = (const charType*)dp->getData();
            memcpy(buffer, qp, M * sizeof(charType));
            buffer[M] = 0;
            str = buffer;
            delete[] buffer;
        }
        else
        {
            if (dp->dataClass != NLS_CHAR)
            {
                throw Exception(_W("Unable to convert supplied object to a string."));
            }
            if (!isRowVector())
            {
                throw Exception(_W("Unable to convert supplied object to a single string."));
            }
        }
        return str;
    }
    //=============================================================================
    stringVector ArrayOf::getContentAsCStringVector(bool bCheckVector) const
    {
        wstringVector wres = getContentAsWideStringVector(bCheckVector);
        stringVector res;
        res.reserve(wres.size());
        for (size_t k = 0; k < wres.size(); k++)
        {
            res.push_back(wstring_to_utf8(wres[k]));
        }
        return res;
    }
    //=============================================================================
    wstringVector ArrayOf::getContentAsWideStringVector(bool bCheckVector) const
    {
        wstringVector res;
        if (dp->dataClass == NLS_CHAR)
        {
            if (is2D())
            {
                indexType rows = getDimensions().getRows();
                indexType columns = getDimensions().getColumns();
                for (indexType i = 0; i < rows; i++)
                {
                    std::wstring str;
                    const charType *qp = nullptr;
                    qp = (const charType*)dp->getData();
                    for (indexType j = 0; j < columns; j++)
                    {
                        size_t idx = i + j * rows;
                        str.push_back(qp[idx]);
                    }
                    res.push_back(str);
                    str.clear();
                }
            }
            else
            {
                res.push_back(getContentAsWideString());
            }
        }
        else
        {
            if (dp->dataClass != NLS_CELL_ARRAY)
            {
                throw Exception(_W("A cell expected."));
            }
            else if (isEmpty())
            {
                //                res.push_back(L"");
            }
            else if (isVector() || !bCheckVector)
            {
                ArrayOf *arg = (ArrayOf*)(getDataPointer());
                indexType nbElements = getDimensions().getElementCount();
                res.reserve(nbElements);
                for (indexType k = 0; k < nbElements; k++)
                {
                    if (arg[k].getDataClass() != NLS_CHAR)
                    {
                        res.clear();
                        throw Exception(_W("A cell of string expected."));
                    }
                    else
                    {
                        res.push_back(arg[k].getContentAsWideString());
                    }
                }
            }
            else
            {
                throw Exception(_W("A vector expected."));
            }
        }
        return res;
    }
    //=============================================================================
    stringVector ArrayOf::getContentAsCStringRowVector(void) const
    {
        wstringVector wres = getContentAsWideStringRowVector();
        stringVector res;
        res.reserve(wres.size());
        for (size_t k = 0; k < wres.size(); k++)
        {
            res.push_back(wstring_to_utf8(wres[k]));
        }
        return res;
    }
    //=============================================================================
    wstringVector ArrayOf::getContentAsWideStringRowVector(void) const
    {
        wstringVector res;
        if (dp->dataClass != NLS_CELL_ARRAY)
        {
            throw Exception(_W("A cell expected."));
        }
        if (isRowVector())
        {
            ArrayOf *arg = (ArrayOf*)(getDataPointer());
            indexType nbElements = getDimensions().getElementCount();
            res.reserve(nbElements);
            for (indexType k = 0; k < nbElements; k++)
            {
                if (arg[k].getDataClass() != NLS_CHAR)
                {
                    res.clear();
                    throw Exception(_W("A cell of string expected."));
                }
                else
                {
                    res.push_back(arg[k].getContentAsWideString());
                }
            }
        }
        else
        {
            throw Exception(_W("A row vector expected."));
        }
        return res;
    }
    //=============================================================================
    stringVector ArrayOf::getContentAsCStringColumnVector(void) const
    {
        wstringVector wres = getContentAsWideStringColumnVector();
        stringVector res;
        res.reserve(wres.size());
        for (size_t k = 0; k < wres.size(); k++)
        {
            res.push_back(wstring_to_utf8(wres[k]));
        }
        return res;
    }
    //=============================================================================
    wstringVector ArrayOf::getContentAsWideStringColumnVector(void) const
    {
        wstringVector res;
        if (dp->dataClass != NLS_CELL_ARRAY)
        {
            throw Exception(_W("A cell expected."));
        }
        if (isColumnVector())
        {
            ArrayOf *arg = (ArrayOf*)(getDataPointer());
            indexType nbElements = getDimensions().getElementCount();
            res.reserve(nbElements);
            for (indexType k = 0; k < nbElements; k++)
            {
                if (arg[k].getDataClass() != NLS_CHAR)
                {
                    res.clear();
                    throw Exception(_W("A cell of string expected."));
                }
                else
                {
                    res.push_back(arg[k].getContentAsWideString());
                }
            }
        }
        else
        {
            throw Exception(_W("A column vector expected."));
        }
        return res;
    }
    //=============================================================================
}
//=============================================================================
