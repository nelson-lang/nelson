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
#include "Fft.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "FftHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Fft(ArrayOf X, indexType n, indexType dim)
{
    if (X.isReferenceType() || X.isHandle()) {
        Error(
            _("Undefined function 'fft' for input arguments of type") + " '" + ClassName(X) + "'.");
    }
    if (X.isScalar() || X.isEmpty()) {
        return ArrayOf(X);
    }
    Class classX = X.getDataClass();
    if (classX <= NLS_SCOMPLEX && classX != NLS_DOUBLE) {
        X.promoteType(NLS_SCOMPLEX);
    } else {
        X.promoteType(NLS_DCOMPLEX);
    }
    ArrayOf res;
    if (X.getDataClass() == NLS_SCOMPLEX) {
        res = scomplexFFTW(X, n, dim, false);
    } else {
        res = dcomplexFFTW(X, n, dim, false);
    }
    return res;
}
//=============================================================================
ArrayOf
Fft(ArrayOf X, indexType n)
{
    indexType dim = computeDim(X);
    return Fft(X, n, dim);
}
//=============================================================================
ArrayOf
Fft(ArrayOf X)
{
    indexType dim = computeDim(X);
    indexType n = X.getDimensionLength((int)dim);
    return Fft(X, n, dim);
}
//=============================================================================
}
//=============================================================================
