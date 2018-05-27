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
#include "IJVBuiltin.hpp"
#include "NelsonGateway.hpp"
#include "fullBuiltin.hpp"
#include "nnzBuiltin.hpp"
#include "nzmaxBuiltin.hpp"
#include "sparseBuiltin.hpp"
#include "sparsedouble_ctransposeBuiltin.hpp"
#include "sparsedouble_dispBuiltin.hpp"
#include "sparsedouble_horzcat_sparsedoubleBuiltin.hpp"
#include "sparsedouble_imagBuiltin.hpp"
#include "sparsedouble_realBuiltin.hpp"
#include "sparsedouble_transposeBuiltin.hpp"
#include "sparsedouble_uminusBuiltin.hpp"
#include "sparsedouble_vertcat_sparsedoubleBuiltin.hpp"
#include "sparselogical_ctransposeBuiltin.hpp"
#include "sparselogical_dispBuiltin.hpp"
#include "sparselogical_horzcat_sparselogicalBuiltin.hpp"
#include "sparselogical_imagBuiltin.hpp"
#include "sparselogical_realBuiltin.hpp"
#include "sparselogical_transposeBuiltin.hpp"
#include "sparselogical_uminusBuiltin.hpp"
#include "sparselogical_vertcat_sparselogicalBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"sparse";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sparse", Nelson::SparseGateway::sparseBuiltin, 1, 6 },
    { "sparsedouble_disp", Nelson::SparseGateway::sparsedouble_dispBuiltin, 0, 1 },
    { "sparselogical_disp", Nelson::SparseGateway::sparselogical_dispBuiltin, 0, 1 },
    { "full", Nelson::SparseGateway::fullBuiltin, 1, 1 },
    { "sparsedouble_uminus", Nelson::SparseGateway::sparsedouble_uminusBuiltin, 1, 1 },
    { "sparselogical_uminus", Nelson::SparseGateway::sparselogical_uminusBuiltin, 1, 1 },

    { "sparselogical_real", Nelson::SparseGateway::sparselogical_realBuiltin, 1, 1 },
    { "sparselogical_imag", Nelson::SparseGateway::sparselogical_imagBuiltin, 1, 1 },
    { "sparsedouble_real", Nelson::SparseGateway::sparsedouble_realBuiltin, 1, 1 },
    { "sparsedouble_imag", Nelson::SparseGateway::sparsedouble_imagBuiltin, 1, 1 },
    { "IJV", Nelson::SparseGateway::IJVBuiltin, -1, 1 },
    { "sparselogical_vertcat_sparselogical",
        Nelson::SparseGateway::sparselogical_vertcat_sparselogicalBuiltin, 1, 2 },
    { "sparselogical_horzcat_sparselogical",
        Nelson::SparseGateway::sparselogical_horzcat_sparselogicalBuiltin, 1, 2 },
    { "sparsedouble_vertcat_sparsedouble",
        Nelson::SparseGateway::sparsedouble_vertcat_sparsedoubleBuiltin, 1, 2 },
    { "sparsedouble_horzcat_sparsedouble",
        Nelson::SparseGateway::sparsedouble_horzcat_sparsedoubleBuiltin, 1, 2 },
    { "sparselogical_ctranspose", Nelson::SparseGateway::sparselogical_ctransposeBuiltin, 1, 1 },
    { "sparsedouble_ctranspose", Nelson::SparseGateway::sparsedouble_ctransposeBuiltin, 1, 1 },
    { "nnz", Nelson::SparseGateway::nnzBuiltin, 1, 1 },
    { "nzmax", Nelson::SparseGateway::nzmaxBuiltin, 1, 1 },
    { "sparselogical_transpose", Nelson::SparseGateway::sparselogical_transposeBuiltin, 1, 1 },
    { "sparsedouble_transpose", Nelson::SparseGateway::sparsedouble_transposeBuiltin, 1, 1 },

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
