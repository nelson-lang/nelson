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
#include <string.h>
//=============================================================================
#define FULLNAME "fullname"
#define DATE "date"
//=============================================================================
struct agenda {
    const char* fullname;
    double date;
};
//=============================================================================
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    const char* field_names[] = {FULLNAME, DATE};
    struct agenda contacts[] = {{"Michael B.", 11122016},
                                  {"Pierre P.",  11122017},
                                  {"Nicolas M.", 11122018},
                                  {"Manu T.",   11122019}};
    mwIndex i = 0;
    mwIndex numberOfStructs = sizeof(contacts) / sizeof(struct agenda);
    mwSize dims[2] = {numberOfStructs, 1};
    int fullnameIndex = 0;
    int dateIndex = 0;

    if (nlhs > 1) {
        mexErrMsgTxt("Too many output arguments.");
    }
    if (nrhs != 0) {
        mexErrMsgTxt("No input argument required.");
    }

    plhs[0] = mxCreateStructArray(2, dims, sizeof(field_names) / sizeof(*field_names), field_names);
    dateIndex = mxGetFieldNumber(plhs[0], DATE);
    fullnameIndex = mxGetFieldNumber(plhs[0], FULLNAME);

    for (i = 0; i < numberOfStructs; i++) {
        mxArray* value = mxCreateDoubleMatrix(1, 1, mxREAL);
        if (value) {
            *mxGetPr(value) = contacts[i].date;
            mxSetFieldByNumber(plhs[0], i, fullnameIndex, mxCreateString(contacts[i].fullname));
            mxSetFieldByNumber(plhs[0], i, dateIndex, value);
        }
    }
}
//=============================================================================
