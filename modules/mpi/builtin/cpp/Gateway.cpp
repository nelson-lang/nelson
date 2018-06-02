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
    { "MPI_Initialized", Nelson::MpiGateway::MPI_InitializedBuiltin, 1, 0 },
    { "MPI_Finalize", Nelson::MpiGateway::MPI_FinalizeBuiltin, 0, 0 },
    { "MPI_Init", Nelson::MpiGateway::MPI_InitBuiltin, 0, 0 },
    { "MPI_Get_processor_name", Nelson::MpiGateway::MPI_Get_processor_nameBuiltin, -1, 0 },
    { "MPI_Get_version", Nelson::MpiGateway::MPI_Get_versionBuiltin, -1, 0 },
    { "MPI_Get_library_version", Nelson::MpiGateway::MPI_Get_library_versionBuiltin, 1, 0 },
    { "MPI_Comm_object", Nelson::MpiGateway::MPI_Comm_objectBuiltin, 1, 1 },
    { "MPI_Comm_disp", Nelson::MpiGateway::MPI_Comm_dispBuiltin, 0, 1 },
    { "MPI_Recv", Nelson::MpiGateway::MPI_RecvBuiltin, 1, 4 },
    { "MPI_Send", Nelson::MpiGateway::MPI_SendBuiltin, 0, 3 },
    { "MPI_Comm_rank", Nelson::MpiGateway::MPI_Comm_rankBuiltin, 1, 1 },
    { "MPI_Comm_size", Nelson::MpiGateway::MPI_Comm_sizeBuiltin, 1, 1 },
    { "MPI_Barrier", Nelson::MpiGateway::MPI_BarrierBuiltin, 1, 1 },
    { "MPI_Bcast", Nelson::MpiGateway::MPI_BcastBuiltin, 1, 2 },
    { "MPI_Reduce", Nelson::MpiGateway::MPI_ReduceBuiltin, 1, 4 },
    { "MPI_Allreduce", Nelson::MpiGateway::MPI_AllreduceBuiltin, 1, 3 },
    { "MPI_Probe", Nelson::MpiGateway::MPI_ProbeBuiltin, 1, 2 },
    { "MPI_Iprobe", Nelson::MpiGateway::MPI_IprobeBuiltin, 1, 3 },
    { "MPI_Comm_get_name", Nelson::MpiGateway::MPI_Comm_get_nameBuiltin, 1, 1 },
    { "MPI_Comm_delete", Nelson::MpiGateway::MPI_Comm_deleteBuiltin, 0, 1 },
    { "MPI_Comm_used", Nelson::MpiGateway::MPI_Comm_usedBuiltin, 1, 0 },
    { "MPI_Comm_isvalid", Nelson::MpiGateway::MPI_Comm_isvalidBuiltin, 0, 1 },
    { "MPI_Comm_split", Nelson::MpiGateway::MPI_Comm_splitBuiltin, 1, 3 },
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
