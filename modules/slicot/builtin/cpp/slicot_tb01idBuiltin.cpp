//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Generated by Nelson Interface Generator 1.0.0
//=============================================================================
#include <algorithm>
#include "slicot_tb01idBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    extern int
    tb01id_(const char* JOB, int* N, int* M, int* P, double* MAXRED, double* A, int* LDA, double* B,
        int* LDB, double* C, int* LDC, double* SCALE, int* INFO);
#ifdef __cplusplus
}
#endif
//=============================================================================
// [MAXRED_OUT, A_OUT, B_OUT, C_OUT, SCALE, INFO] = slicot_tb01id(JOB, MAXRED_IN, A_IN, B_IN, C_IN)
//=============================================================================
ArrayOfVector
Nelson::SlicotGateway::slicot_tb01idBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 6);
    nargincheck(argIn, 5, 5);
    // INPUT VARIABLES
    ArrayOf JOB = argIn[0];
    Dimensions dimsJOB = JOB.getDimensions();
    std::string JOB_string = JOB.getContentAsCString();
    const char* JOB_ptr = JOB_string.c_str();
    // IN/OUT VARIABLES
    ArrayOf MAXRED = argIn[1];
    Dimensions dimsMAXRED = MAXRED.getDimensions();
    MAXRED.promoteType(NLS_DOUBLE);
    ArrayOf MAXRED_output = MAXRED;
    MAXRED_output.ensureSingleOwner();
    double* MAXRED_output_ptr = (double*)MAXRED_output.getDataPointer();
    ArrayOf A = argIn[2];
    Dimensions dimsA = A.getDimensions();
    A.promoteType(NLS_DOUBLE);
    ArrayOf A_output = A;
    A_output.ensureSingleOwner();
    double* A_output_ptr = (double*)A_output.getDataPointer();
    ArrayOf B = argIn[3];
    Dimensions dimsB = B.getDimensions();
    B.promoteType(NLS_DOUBLE);
    ArrayOf B_output = B;
    B_output.ensureSingleOwner();
    double* B_output_ptr = (double*)B_output.getDataPointer();
    ArrayOf C = argIn[4];
    Dimensions dimsC = C.getDimensions();
    C.promoteType(NLS_DOUBLE);
    ArrayOf C_output = C;
    C_output.ensureSingleOwner();
    double* C_output_ptr = (double*)C_output.getDataPointer();
    // LOCAL VARIABLES
    ArrayOf N = ArrayOf::int32RowVectorConstructor(1);
    int* N_ptr = (int*)N.getDataPointer();
    N_ptr[0] = (int)B.getRows();
    ArrayOf M = ArrayOf::int32RowVectorConstructor(1);
    int* M_ptr = (int*)M.getDataPointer();
    M_ptr[0] = (int)B.getColumns();
    ArrayOf P = ArrayOf::int32RowVectorConstructor(1);
    int* P_ptr = (int*)P.getDataPointer();
    P_ptr[0] = (int)C.getColumns();
    ArrayOf LDA = ArrayOf::int32RowVectorConstructor(1);
    int* LDA_ptr = (int*)LDA.getDataPointer();
    LDA_ptr[0] = std::max(1, (int)N.getContentAsInteger32Scalar());
    ArrayOf LDB = ArrayOf::int32RowVectorConstructor(1);
    int* LDB_ptr = (int*)LDB.getDataPointer();
    LDB_ptr[0] = std::max(1, (int)N.getContentAsInteger32Scalar());
    ArrayOf LDC = ArrayOf::int32RowVectorConstructor(1);
    int* LDC_ptr = (int*)LDC.getDataPointer();
    LDC_ptr[0] = std::max(1, (int)P.getContentAsInteger32Scalar());
    // OUTPUT VARIABLES
    ArrayOf SCALE_output = ArrayOf::doubleMatrix2dConstructor(
        (indexType)1, (indexType)(int)N.getContentAsInteger32Scalar());
    double* SCALE_output_ptr = (double*)SCALE_output.getDataPointer();
    ArrayOf INFO_output = ArrayOf::int32RowVectorConstructor(1);
    int* INFO_output_ptr = (int*)INFO_output.getDataPointer();
    // CHECK INPUT VARIABLES DIMENSIONS
    if (!dimsJOB.isScalar()) {
        Error(_W("Input argument #1: scalar expected."));
    }
    if (!dimsMAXRED.isScalar()) {
        Error(_W("Input argument #2: scalar expected."));
    }
    Dimensions dimsA_expected(
        std::max(1, (int)N.getContentAsInteger32Scalar()), (int)N.getContentAsInteger32Scalar());
    if (!dimsA.equals(dimsA_expected)) {
        Error(_("Input argument #3: wrong size.") + " " + dimsA_expected.toString() + " "
            + "expected" + ".");
    }
    Dimensions dimsB_expected(
        std::max(1, (int)N.getContentAsInteger32Scalar()), (int)M.getContentAsInteger32Scalar());
    if (!dimsB.equals(dimsB_expected)) {
        Error(_("Input argument #4: wrong size.") + " " + dimsB_expected.toString() + " "
            + "expected" + ".");
    }
    Dimensions dimsC_expected(
        std::max(1, (int)P.getContentAsInteger32Scalar()), (int)N.getContentAsInteger32Scalar());
    if (!dimsC.equals(dimsC_expected)) {
        Error(_("Input argument #5: wrong size.") + " " + dimsC_expected.toString() + " "
            + "expected" + ".");
    }
    // CALL EXTERN FUNCTION
    try {
        tb01id_(JOB_ptr, N_ptr, M_ptr, P_ptr, MAXRED_output_ptr, A_output_ptr, LDA_ptr,
            B_output_ptr, LDB_ptr, C_output_ptr, LDC_ptr, SCALE_output_ptr, INFO_output_ptr);
    } catch (const std::runtime_error&) {
        Error("tb01id function fails.");
    }
    // ASSIGN OUTPUT VARIABLES
    if (nLhs > 0) {
        retval << MAXRED_output;
    }
    if (nLhs > 1) {
        retval << A_output;
    }
    if (nLhs > 2) {
        retval << B_output;
    }
    if (nLhs > 3) {
        retval << C_output;
    }
    if (nLhs > 4) {
        retval << SCALE_output;
    }
    if (nLhs > 5) {
        retval << INFO_output;
    }
    return retval;
}
//=============================================================================
