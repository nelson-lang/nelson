//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "NelsonGateway.hpp"
#include "MPI_InitializedBuiltin.hpp"
#include "MPI_FinalizeBuiltin.hpp"
#include "MPI_InitBuiltin.hpp"
#include "MPI_Get_processor_nameBuiltin.hpp"
#include "MPI_Get_versionBuiltin.hpp"
#include "MPI_Get_library_versionBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"mpi";
//=============================================================================
static const nlsGateway gateway[] =
{
	{ "MPI_Initialized", Nelson::MpiGateway::MPI_InitializedBuiltin, 0, 0 },
	{ "MPI_Finalize", Nelson::MpiGateway::MPI_FinalizeBuiltin, 0, 0 },
	{ "MPI_Init", Nelson::MpiGateway::MPI_InitBuiltin, 0, 0 },
	{ "MPI_Get_processor_name", Nelson::MpiGateway::MPI_Get_processor_nameBuiltin, -1, 0 },
	{ "MPI_Get_version", Nelson::MpiGateway::MPI_Get_versionBuiltin, -1, 0},
	{ "MPI_Get_library_version", Nelson::MpiGateway::MPI_Get_library_versionBuiltin, 1, 0},
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
