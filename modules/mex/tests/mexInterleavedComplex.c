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
  mxArray *v = mxCreateDoubleMatrix (1, 1, mxCOMPLEX);
#if defined (MX_HAS_INTERLEAVED_COMPLEX)
  mxComplexDouble *data = mxGetComplexDoubles (v);
  data->real = 1.53;
  data->imag = 1.63;
#else
  double *re_data = mxGetPr(v);
  double *im_data = mxGetPi(v);
  *re_data = 1.73;
  *im_data = 4.76;
#endif
  plhs[0] = v;
  int i;
  for (i = 1; i < nlhs; i++) {
      plhs[i] = mxCreateDoubleMatrix (0, 0, mxREAL);
  }
}
//=============================================================================
