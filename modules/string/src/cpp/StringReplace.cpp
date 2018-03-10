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
#include <boost/algorithm/string.hpp>
#include "StringReplace.hpp"
#include "IsCellOfStrings.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static bool isSameSize(ArrayOf A, ArrayOf B)
    {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        dimsA.simplify();
        dimsB.simplify();
        return (dimsA.equals(dimsB));
    }
    //=============================================================================
	ArrayOf StringReplace(ArrayOf STR, ArrayOf OLD, ArrayOf NEW)
    {
        ArrayOf res;
        size_t nbOutput;
        wstringVector wstr = STR.getContentAsWideStringVector(false, true);
        wstringVector wold = OLD.getContentAsWideStringVector(false, true);
        wstringVector wnew = NEW.getContentAsWideStringVector(false, true);
		Dimensions outputDims;
        if (wstr.size() == 1 && wold.size() == 1)
        {
            nbOutput = wnew.size();
            outputDims = NEW.getDimensions();
        }
        else if (wstr.size() == 1 && wnew.size() == 1)
        {
            nbOutput = wold.size();
            outputDims = OLD.getDimensions();
        }
        else if (wold.size() == 1 && wnew.size() == 1)
        {
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        }
        else if (wstr.size() == 1)
        {
            if (!OLD.getDimensions().equals(NEW.getDimensions()))
            {
                throw Exception(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wold.size();
            outputDims = OLD.getDimensions();
        }
        else if (wold.size() == 1)
        {
            if (!STR.getDimensions().equals(NEW.getDimensions()))
            {
                throw Exception(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        }
        else if (wnew.size() == 1)
        {
            if (!STR.getDimensions().equals(OLD.getDimensions()))
            {
                throw Exception(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        }
        else
        {
            if ((!STR.getDimensions().equals(OLD.getDimensions())) ||
                    (!STR.getDimensions().equals(NEW.getDimensions())))
            {
                throw Exception(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        }
        if (nbOutput == 1)
        {
            std::wstring r = boost::replace_all_copy(wstr[0], wold[0], wnew[0]);
            res = ArrayOf::stringConstructor(r);
        }
        else
        {
            int str_incr = (wstr.size() == 1 ? 0 : 1);
            int old_incr = (wold.size() == 1 ? 0 : 1);
            int new_incr = (wnew.size() == 1 ? 0 : 1);
            ArrayOf *elements = nullptr;
            try
            {
                elements = new ArrayOf[nbOutput];
            }
            catch (std::bad_alloc &e)
            {
                e.what();
                throw Exception(ERROR_MEMORY_ALLOCATION);
            }
            for (size_t i = 0; i < nbOutput; i++)
            {
                size_t idx_str = (i) * str_incr;
                size_t idx_old = (i) * old_incr;
                size_t idx_new = (i) * new_incr;
				boost::replace_all(wstr[idx_str], wold[idx_old], wnew[idx_new]);
                elements[i] = ArrayOf::stringConstructor(wstr[idx_str]);
            }
            res = ArrayOf(NLS_CELL_ARRAY, outputDims, elements);
        }
        return res;
    }
    //=============================================================================
}
//=============================================================================
