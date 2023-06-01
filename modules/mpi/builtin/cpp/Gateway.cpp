//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_AllreduceBuiltin.hpp"
#include "MPI_BarrierBuiltin.hpp"
#include "MPI_BcastBuiltin.hpp"
#include "MPI_Comm_deleteBuiltin.hpp"
#include "MPI_Comm_displayBuiltin.hpp"
#include "MPI_Comm_get_nameBuiltin.hpp"
#include "MPI_Comm_isvalidBuiltin.hpp"
#include "MPI_Comm_objectBuiltin.hpp"
#include "MPI_Comm_rankBuiltin.hpp"
#include "MPI_Comm_sizeBuiltin.hpp"
#include "MPI_Comm_splitBuiltin.hpp"
#include "MPI_Comm_usedBuiltin.hpp"
#include "MPI_FinalizeBuiltin.hpp"
#include "MPI_Get_library_versionBuiltin.hpp"
#include "MPI_Get_processor_nameBuiltin.hpp"
#include "MPI_Get_versionBuiltin.hpp"
#include "MPI_InitBuiltin.hpp"
#include "MPI_InitializedBuiltin.hpp"
#include "MPI_IprobeBuiltin.hpp"
#include "MPI_ProbeBuiltin.hpp"
#include "MPI_RecvBuiltin.hpp"
#include "MPI_ReduceBuiltin.hpp"
#include "MPI_SendBuiltin.hpp"
#include "NelsonGateway.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"mpi";
//=============================================================================
static const nlsGateway gateway[] = {
    { "MPI_Init", (ptrBuiltin)Nelson::MpiGateway::MPI_InitBuiltin, 0, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "display"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_displayBuiltin, 0, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "disp"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_displayBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "isvalid"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_isvalidBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "MPI_Initialized", (ptrBuiltin)Nelson::MpiGateway::MPI_InitializedBuiltin, 1, 0 },
    { "MPI_Finalize", (ptrBuiltin)Nelson::MpiGateway::MPI_FinalizeBuiltin, 0, 0 },
    { "MPI_Get_processor_name", (ptrBuiltin)Nelson::MpiGateway::MPI_Get_processor_nameBuiltin, -1,
        0 },
    { "MPI_Get_version", (ptrBuiltin)Nelson::MpiGateway::MPI_Get_versionBuiltin, -1, 0 },
    { "MPI_Get_library_version", (ptrBuiltin)Nelson::MpiGateway::MPI_Get_library_versionBuiltin, 1,
        0 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "object"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_objectBuiltin, 1, 1 },
    { "MPI_Recv", (ptrBuiltin)Nelson::MpiGateway::MPI_RecvBuiltin, 1, 4 },
    { "MPI_Send", (ptrBuiltin)Nelson::MpiGateway::MPI_SendBuiltin, 0, 3 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "rank"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_rankBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "size"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_sizeBuiltin, 1, 1 },
    { "MPI_Barrier", (ptrBuiltin)Nelson::MpiGateway::MPI_BarrierBuiltin, 1, 1 },
    { "MPI_Bcast", (ptrBuiltin)Nelson::MpiGateway::MPI_BcastBuiltin, 1, 2 },
    { "MPI_Reduce", (ptrBuiltin)Nelson::MpiGateway::MPI_ReduceBuiltin, 1, 4 },
    { "MPI_Allreduce", (ptrBuiltin)Nelson::MpiGateway::MPI_AllreduceBuiltin, 1, 3 },
    { "MPI_Probe", (ptrBuiltin)Nelson::MpiGateway::MPI_ProbeBuiltin, 1, 2 },
    { "MPI_Iprobe", (ptrBuiltin)Nelson::MpiGateway::MPI_IprobeBuiltin, 1, 3 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "get_name"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_get_nameBuiltin, 1, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_deleteBuiltin, 0, 1 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "used"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_usedBuiltin, 1, 0 },
    { OVERLOAD_METHOD_NAME(NLS_HANDLE_MPI_COMM_CATEGORY_STR, "split"),
        (ptrBuiltin)Nelson::MpiGateway::MPI_Comm_splitBuiltin, 1, 3 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
