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
#include "slicot_ag08bdBuiltin.hpp"
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
    ag08bd_(const char* EQUIL, int* L, int* N, int* M, int* P, double* A, int* LDA, double* E,
        int* LDE, const double* B, int* LDB, const double* C, int* LDC, const double* D, int* LDD,
        int* NFZ, int* NRANK, int* NIZ, int* DINFZ, int* NKROR, int* NINFE, int* NKROL, int* INFZ,
        int* KRONR, int* INFE, int* KRONL, double* TOL, int* IWORK, double* DWORK, int* LDWORK,
        int* INFO);
#ifdef __cplusplus
}
#endif
//=============================================================================
// [A_OUT, E_OUT, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE, NKROL, INFZ, KRONR, INFE, KRONL, INFO] =
// slicot_ag08bd(EQUIL, M, P, A_IN, E_IN, B, C, D, TOL)
//=============================================================================
ArrayOfVector
Nelson::SlicotGateway::slicot_ag08bdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 14);
    nargincheck(argIn, 9, 9);
    // INPUT VARIABLES
    ArrayOf EQUIL = argIn[0];
    Dimensions dimsEQUIL = EQUIL.getDimensions();
    std::string EQUIL_string = EQUIL.getContentAsCString();
    const char* EQUIL_ptr = EQUIL_string.c_str();
    ArrayOf M = argIn[1];
    Dimensions dimsM = M.getDimensions();
    M.promoteType(NLS_INT32);
    int* M_ptr = (int*)M.getDataPointer();
    ArrayOf P = argIn[2];
    Dimensions dimsP = P.getDimensions();
    P.promoteType(NLS_INT32);
    int* P_ptr = (int*)P.getDataPointer();
    ArrayOf B = argIn[5];
    Dimensions dimsB = B.getDimensions();
    B.promoteType(NLS_DOUBLE);
    double* B_ptr = (double*)B.getDataPointer();
    ArrayOf C = argIn[6];
    Dimensions dimsC = C.getDimensions();
    C.promoteType(NLS_DOUBLE);
    double* C_ptr = (double*)C.getDataPointer();
    ArrayOf D = argIn[7];
    Dimensions dimsD = D.getDimensions();
    D.promoteType(NLS_DOUBLE);
    double* D_ptr = (double*)D.getDataPointer();
    ArrayOf TOL = argIn[8];
    Dimensions dimsTOL = TOL.getDimensions();
    TOL.promoteType(NLS_DOUBLE);
    double* TOL_ptr = (double*)TOL.getDataPointer();
    // IN/OUT VARIABLES
    ArrayOf A = argIn[3];
    Dimensions dimsA = A.getDimensions();
    A.promoteType(NLS_DOUBLE);
    ArrayOf A_output = A;
    A_output.ensureSingleOwner();
    double* A_output_ptr = (double*)A_output.getDataPointer();
    ArrayOf E = argIn[4];
    Dimensions dimsE = E.getDimensions();
    E.promoteType(NLS_DOUBLE);
    ArrayOf E_output = E;
    E_output.ensureSingleOwner();
    double* E_output_ptr = (double*)E_output.getDataPointer();
    // LOCAL VARIABLES
    ArrayOf L = ArrayOf::int32RowVectorConstructor(1);
    int* L_ptr = (int*)L.getDataPointer();
    L_ptr[0] = (int)A.getRows();
    ArrayOf N = ArrayOf::int32RowVectorConstructor(1);
    int* N_ptr = (int*)N.getDataPointer();
    N_ptr[0] = (int)A.getColumns();
    ArrayOf LDA = ArrayOf::int32RowVectorConstructor(1);
    int* LDA_ptr = (int*)LDA.getDataPointer();
    LDA_ptr[0] = std::max(1, (int)L.getContentAsInteger32Scalar());
    ArrayOf LDE = ArrayOf::int32RowVectorConstructor(1);
    int* LDE_ptr = (int*)LDE.getDataPointer();
    LDE_ptr[0] = std::max(1, (int)L.getContentAsInteger32Scalar());
    ArrayOf LDB = ArrayOf::int32RowVectorConstructor(1);
    int* LDB_ptr = (int*)LDB.getDataPointer();
    LDB_ptr[0] = std::max(1, (int)L.getContentAsInteger32Scalar());
    ArrayOf LDC = ArrayOf::int32RowVectorConstructor(1);
    int* LDC_ptr = (int*)LDC.getDataPointer();
    LDC_ptr[0] = std::max(1, (int)P.getContentAsInteger32Scalar());
    ArrayOf LDD = ArrayOf::int32RowVectorConstructor(1);
    int* LDD_ptr = (int*)LDD.getDataPointer();
    LDD_ptr[0] = std::max(1, (int)P.getContentAsInteger32Scalar());
    ArrayOf IWORK = ArrayOf::int32Matrix2dConstructor(1,
        (int)N.getContentAsInteger32Scalar() + std::max(1, (int)M.getContentAsInteger32Scalar()));
    int* IWORK_ptr = (int*)IWORK.getDataPointer();
    ArrayOf DWORK = ArrayOf::doubleMatrix2dConstructor(1,
        (int)(std::max(
            4 * ((int)L.getContentAsInteger32Scalar() + (int)N.getContentAsInteger32Scalar()),
            std::max((int)P.getContentAsInteger32Scalar() + (int)L.getContentAsInteger32Scalar(),
                (int)M.getContentAsInteger32Scalar() + (int)N.getContentAsInteger32Scalar())
                    * std::max(
                        (int)P.getContentAsInteger32Scalar() + (int)L.getContentAsInteger32Scalar(),
                        (int)M.getContentAsInteger32Scalar() + (int)N.getContentAsInteger32Scalar())
                + std::max(1,
                    5
                        * std::max((int)P.getContentAsInteger32Scalar()
                                + (int)L.getContentAsInteger32Scalar(),
                            (int)M.getContentAsInteger32Scalar()
                                + (int)N.getContentAsInteger32Scalar())))));
    double* DWORK_ptr = (double*)DWORK.getDataPointer();
    ArrayOf LDWORK = ArrayOf::int32RowVectorConstructor(1);
    int* LDWORK_ptr = (int*)LDWORK.getDataPointer();
    LDWORK_ptr[0] = (int)(std::max(
        4 * ((int)L.getContentAsInteger32Scalar() + (int)N.getContentAsInteger32Scalar()),
        std::max((int)P.getContentAsInteger32Scalar() + (int)L.getContentAsInteger32Scalar(),
            (int)M.getContentAsInteger32Scalar() + (int)N.getContentAsInteger32Scalar())
                * std::max(
                    (int)P.getContentAsInteger32Scalar() + (int)L.getContentAsInteger32Scalar(),
                    (int)M.getContentAsInteger32Scalar() + (int)N.getContentAsInteger32Scalar())
            + std::max(1,
                5
                    * std::max(
                        (int)P.getContentAsInteger32Scalar() + (int)L.getContentAsInteger32Scalar(),
                        (int)M.getContentAsInteger32Scalar()
                            + (int)N.getContentAsInteger32Scalar()))));
    // OUTPUT VARIABLES
    ArrayOf NFZ_output = ArrayOf::int32RowVectorConstructor(1);
    int* NFZ_output_ptr = (int*)NFZ_output.getDataPointer();
    ArrayOf NRANK_output = ArrayOf::int32RowVectorConstructor(1);
    int* NRANK_output_ptr = (int*)NRANK_output.getDataPointer();
    ArrayOf NIZ_output = ArrayOf::int32RowVectorConstructor(1);
    int* NIZ_output_ptr = (int*)NIZ_output.getDataPointer();
    ArrayOf DINFZ_output = ArrayOf::int32RowVectorConstructor(1);
    int* DINFZ_output_ptr = (int*)DINFZ_output.getDataPointer();
    ArrayOf NKROR_output = ArrayOf::int32RowVectorConstructor(1);
    int* NKROR_output_ptr = (int*)NKROR_output.getDataPointer();
    ArrayOf NINFE_output = ArrayOf::int32RowVectorConstructor(1);
    int* NINFE_output_ptr = (int*)NINFE_output.getDataPointer();
    ArrayOf NKROL_output = ArrayOf::int32RowVectorConstructor(1);
    int* NKROL_output_ptr = (int*)NKROL_output.getDataPointer();
    ArrayOf INFZ_output = ArrayOf::int32Matrix2dConstructor(
        (indexType)1, (indexType)(int)N.getContentAsInteger32Scalar() + 1);
    int* INFZ_output_ptr = (int*)INFZ_output.getDataPointer();
    ArrayOf KRONR_output = ArrayOf::int32Matrix2dConstructor((indexType)1,
        (indexType)(int)N.getContentAsInteger32Scalar() + (int)M.getContentAsInteger32Scalar() + 1);
    int* KRONR_output_ptr = (int*)KRONR_output.getDataPointer();
    ArrayOf INFE_output = ArrayOf::int32Matrix2dConstructor((indexType)1,
        (indexType)1
            + std::min((int)L.getContentAsInteger32Scalar() + (int)P.getContentAsInteger32Scalar(),
                (int)N.getContentAsInteger32Scalar() + (int)M.getContentAsInteger32Scalar()));
    int* INFE_output_ptr = (int*)INFE_output.getDataPointer();
    ArrayOf KRONL_output = ArrayOf::int32Matrix2dConstructor((indexType)1,
        (indexType)(int)L.getContentAsInteger32Scalar() + (int)P.getContentAsInteger32Scalar() + 1);
    int* KRONL_output_ptr = (int*)KRONL_output.getDataPointer();
    ArrayOf INFO_output = ArrayOf::int32RowVectorConstructor(1);
    int* INFO_output_ptr = (int*)INFO_output.getDataPointer();
    // CHECK INPUT VARIABLES DIMENSIONS
    if (!dimsEQUIL.isScalar()) {
        Error(_W("Input argument #1: scalar expected."));
    }
    if (!dimsM.isScalar()) {
        Error(_W("Input argument #2: scalar expected."));
    }
    if (!dimsP.isScalar()) {
        Error(_W("Input argument #3: scalar expected."));
    }
    Dimensions dimsA_expected(
        std::max(1, (int)L.getContentAsInteger32Scalar()), (int)N.getContentAsInteger32Scalar());
    if (!dimsA.equals(dimsA_expected)) {
        Error(_("Input argument #4: wrong size.") + " " + dimsA_expected.toString() + " "
            + "expected" + ".");
    }
    Dimensions dimsE_expected(
        std::max(1, (int)L.getContentAsInteger32Scalar()), (int)N.getContentAsInteger32Scalar());
    if (!dimsE.equals(dimsE_expected)) {
        Error(_("Input argument #5: wrong size.") + " " + dimsE_expected.toString() + " "
            + "expected" + ".");
    }
    // DIMENSIONS NOT CHECKED FOR #6 B
    // DIMENSIONS NOT CHECKED FOR #7 C
    // DIMENSIONS NOT CHECKED FOR #8 D
    if (!dimsTOL.isScalar()) {
        Error(_W("Input argument #9: scalar expected."));
    }
    // CALL EXTERN FUNCTION
    try {
        ag08bd_(EQUIL_ptr, L_ptr, N_ptr, M_ptr, P_ptr, A_output_ptr, LDA_ptr, E_output_ptr, LDE_ptr,
            B_ptr, LDB_ptr, C_ptr, LDC_ptr, D_ptr, LDD_ptr, NFZ_output_ptr, NRANK_output_ptr,
            NIZ_output_ptr, DINFZ_output_ptr, NKROR_output_ptr, NINFE_output_ptr, NKROL_output_ptr,
            INFZ_output_ptr, KRONR_output_ptr, INFE_output_ptr, KRONL_output_ptr, TOL_ptr,
            IWORK_ptr, DWORK_ptr, LDWORK_ptr, INFO_output_ptr);
    } catch (const std::runtime_error&) {
        Error("ag08bd function fails.");
    }
    // ASSIGN OUTPUT VARIABLES
    if (nLhs > 0) {
        retval << A_output;
    }
    if (nLhs > 1) {
        retval << E_output;
    }
    if (nLhs > 2) {
        retval << NFZ_output;
    }
    if (nLhs > 3) {
        retval << NRANK_output;
    }
    if (nLhs > 4) {
        retval << NIZ_output;
    }
    if (nLhs > 5) {
        retval << DINFZ_output;
    }
    if (nLhs > 6) {
        retval << NKROR_output;
    }
    if (nLhs > 7) {
        retval << NINFE_output;
    }
    if (nLhs > 8) {
        retval << NKROL_output;
    }
    if (nLhs > 9) {
        retval << INFZ_output;
    }
    if (nLhs > 10) {
        retval << KRONR_output;
    }
    if (nLhs > 11) {
        retval << INFE_output;
    }
    if (nLhs > 12) {
        retval << KRONL_output;
    }
    if (nLhs > 13) {
        retval << INFO_output;
    }
    return retval;
}
//=============================================================================
