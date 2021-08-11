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
#include "MPI_AllreduceBuiltin.hpp"
#include "MPI_BarrierBuiltin.hpp"
#include "MPI_BcastBuiltin.hpp"
#include "MPI_Comm_deleteBuiltin.hpp"
#include "MPI_Comm_dispBuiltin.hpp"
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
    { "MPI_Init", (void*)Nelson::MpiGateway::MPI_InitBuiltin, 0, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "MPI_Comm_display", (void*)Nelson::MpiGateway::MPI_Comm_dispBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "MPI_Comm_isvalid", (void*)Nelson::MpiGateway::MPI_Comm_isvalidBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "MPI_Initialized", (void*)Nelson::MpiGateway::MPI_InitializedBuiltin, 1, 0 },
    { "MPI_Finalize", (void*)Nelson::MpiGateway::MPI_FinalizeBuiltin, 0, 0 },
    { "MPI_Get_processor_name", (void*)Nelson::MpiGateway::MPI_Get_processor_nameBuiltin, -1, 0 },
    { "MPI_Get_version", (void*)Nelson::MpiGateway::MPI_Get_versionBuiltin, -1, 0 },
    { "MPI_Get_library_version", (void*)Nelson::MpiGateway::MPI_Get_library_versionBuiltin, 1, 0 },
    { "MPI_Comm_object", (void*)Nelson::MpiGateway::MPI_Comm_objectBuiltin, 1, 1 },
    { "MPI_Recv", (void*)Nelson::MpiGateway::MPI_RecvBuiltin, 1, 4 },
    { "MPI_Send", (void*)Nelson::MpiGateway::MPI_SendBuiltin, 0, 3 },
    { "MPI_Comm_rank", (void*)Nelson::MpiGateway::MPI_Comm_rankBuiltin, 1, 1 },
    { "MPI_Comm_size", (void*)Nelson::MpiGateway::MPI_Comm_sizeBuiltin, 1, 1 },
    { "MPI_Barrier", (void*)Nelson::MpiGateway::MPI_BarrierBuiltin, 1, 1 },
    { "MPI_Bcast", (void*)Nelson::MpiGateway::MPI_BcastBuiltin, 1, 2 },
    { "MPI_Reduce", (void*)Nelson::MpiGateway::MPI_ReduceBuiltin, 1, 4 },
    { "MPI_Allreduce", (void*)Nelson::MpiGateway::MPI_AllreduceBuiltin, 1, 3 },
    { "MPI_Probe", (void*)Nelson::MpiGateway::MPI_ProbeBuiltin, 1, 2 },
    { "MPI_Iprobe", (void*)Nelson::MpiGateway::MPI_IprobeBuiltin, 1, 3 },
    { "MPI_Comm_get_name", (void*)Nelson::MpiGateway::MPI_Comm_get_nameBuiltin, 1, 1 },
    { "MPI_Comm_delete", (void*)Nelson::MpiGateway::MPI_Comm_deleteBuiltin, 0, 1 },
    { "MPI_Comm_used", (void*)Nelson::MpiGateway::MPI_Comm_usedBuiltin, 1, 0 },
    { "MPI_Comm_split", (void*)Nelson::MpiGateway::MPI_Comm_splitBuiltin, 1, 3 },
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
