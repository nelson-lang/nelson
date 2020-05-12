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
#include <mex.h>
//=============================================================================
void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
#if defined (MX_HAS_INTERLEAVED_COMPLEX)
  mxArray *pa = mxCreateNumericMatrix(2, 1, mxSINGLE_CLASS, mxREAL);
  mxSingle *pd = mxGetSingles(pa);
  mxSingle *dt = mxMalloc(2 * sizeof(mxSingle));
  dt[0] = 33.f;
  dt[1] = 44.f;
  mxFree(pd);  
  mxSetSingles(pa, dt);
  plhs[0] = pa;
  int i;
  for (i = 1; i < nlhs; i++) {
      plhs[i] = mxCreateDoubleMatrix (0, 0, mxREAL);
  }
#endif
}
//=============================================================================
