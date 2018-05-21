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
#include <Eigen/Dense>
#include <cmath>
#include "ColonSingle.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf colon_single(ArrayOf a, ArrayOf b)
    {
        single A = a.getContentAsSingleScalar();
        single B = b.getContentAsSingleScalar();
        return single_colon(A, B);
    }
    //=============================================================================
    ArrayOf colon_single(ArrayOf a, ArrayOf b, ArrayOf c)
    {
        single A = a.getContentAsSingleScalar();
        single B = b.getContentAsSingleScalar();
        single C = c.getContentAsSingleScalar();
        return single_colon(A, B, C);
    }
    //=============================================================================
    ArrayOf single_colon(single low, single high, single step)
    {
        if (step == 0)
        {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(NLS_SINGLE);
            return res;
        }
        if (std::isnan(low) || std::isnan(high) || std::isnan(step))
        {
            Dimensions Cdim(1, 1);
            return ArrayOf::singleConstructor(nanf(""));
        }
        if (!std::isfinite(low) || !std::isfinite(high) || !std::isfinite(step))
        {
            throw Exception(_W("Invalid range."));
        }
        if (low < high)
        {
            if (step < 0)
            {
                ArrayOf res = ArrayOf::emptyConstructor(1, 0);
                res.promoteType(NLS_SINGLE);
                return res;
            }
        }
        if (low > high)
        {
            if (step > 0)
            {
                ArrayOf res = ArrayOf::emptyConstructor(1, 0);
                res.promoteType(NLS_SINGLE);
                return res;
            }
        }
        single dn = (single) ((((high - low) / step) + 1));
        int n = (int)round((single)(dn));
        ArrayOf V = ArrayOf::singleVectorConstructor(n);
        single *pV = (single*)V.getReadWriteDataPointer();
        if (dn == (single(n)))
        {
            Eigen::Map<Eigen::VectorXf> Range(pV, n);
            Range = Eigen::VectorXf::LinSpaced(n, low, high);
        }
        else
        {
            // We must use another algo. in this case
            // 1:2:10
            int i = 0;
            single v = low;
            while ((low < high && v <= high) || (low>high && v >= high))
            {
                pV[i] = v;
                v = v + step;
                i++;
            }
        }
        return V;
    }
    //=============================================================================
}
//=============================================================================
