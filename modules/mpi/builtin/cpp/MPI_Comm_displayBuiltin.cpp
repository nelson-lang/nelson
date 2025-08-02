//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mpi.h>
#include "MPI_Comm_displayBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include "characters_encoding.hpp"
#include "DisplayVariableHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Comm_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf param1 = argIn[0];
    if (eval == nullptr) {
        return retval;
    }
    Interface* io = eval->getInterface();
    if (io == nullptr) {
        return retval;
    }
    std::wstring name;
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_MPI_COMM_CATEGORY_STR) {
                Error(_W("MPI_Comm handle expected."));
            }
            MPI_CommHandleObject* mpicommhandleobj
                = (MPI_CommHandleObject*)param1.getContentAsHandleScalar();
            if (mpicommhandleobj != nullptr) {
                MPI_CommObject* obj = (MPI_CommObject*)mpicommhandleobj->getPointer();
                if (obj != nullptr) {
                    io->outputMessage("\n");
                    std::wstring description = utf8_to_wstring(getMpiCommName(obj->getComm()));
                    io->outputMessage(L"    " + _W("Description") + L":    " + description + L"\n");
                }
            }
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("MPI_Comm handle expected."));
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Comm_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf param1 = argIn[0];
    if (eval == nullptr) {
        return retval;
    }
    Interface* io = eval->getInterface();
    if (io == nullptr) {
        return retval;
    }
    std::wstring name = argIn[0].wname();
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        DisplayVariableHeader(io, param1, name, false);
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != NLS_HANDLE_MPI_COMM_CATEGORY_STR) {
                Error(_W("MPI_Comm handle expected."));
            }
            MPI_CommHandleObject* mpicommhandleobj
                = (MPI_CommHandleObject*)param1.getContentAsHandleScalar();
            if (mpicommhandleobj != nullptr) {
                MPI_CommObject* obj = (MPI_CommObject*)mpicommhandleobj->getPointer();
                if (obj != nullptr) {
                    io->outputMessage("\n");
                    std::wstring description = utf8_to_wstring(getMpiCommName(obj->getComm()));
                    io->outputMessage(L"    " + _W("Description") + L":    " + description + L"\n");
                }
            }
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("MPI_Comm handle expected."));
    }
    return retval;
}
//=============================================================================
