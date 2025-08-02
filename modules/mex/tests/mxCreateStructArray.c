//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mex.h>
#include <string.h>
//=============================================================================
#define FULLNAME "fullname"
#define DATE "date"
//=============================================================================
struct agenda
{
    const char* fullname;
    double date;
};
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    const char* field_names[] = { FULLNAME, DATE };
    struct agenda contacts[] = { { "Michael B.", 11122016 }, { "Pierre P.", 11122017 },
        { "Nicolas M.", 11122018 }, { "Manu T.", 11122019 } };
    mwIndex i = 0;
    mwIndex numberOfStructs = sizeof(contacts) / sizeof(struct agenda);
    mwSize dims[2] = { numberOfStructs, 1 };
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
