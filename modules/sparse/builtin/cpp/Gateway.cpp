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
#include "sparseBuiltin.hpp"
#include "fullBuiltin.hpp"
#include "IJVBuiltin.hpp"
#include "sparsedouble_dispBuiltin.hpp"
#include "sparselogical_dispBuiltin.hpp"
#include "sparsedouble_uminusBuiltin.hpp"
#include "sparselogical_uminusBuiltin.hpp"
#include "sparsedouble_imagBuiltin.hpp"
#include "sparsedouble_realBuiltin.hpp"
#include "sparselogical_imagBuiltin.hpp"
#include "sparselogical_realBuiltin.hpp"
#include "sparsedouble_horzcat_sparsedoubleBuiltin.hpp"
#include "sparsedouble_vertcat_sparsedoubleBuiltin.hpp"
#include "sparselogical_horzcat_sparselogicalBuiltin.hpp"
#include "sparselogical_vertcat_sparselogicalBuiltin.hpp"
#include "sparselogical_ctransposeBuiltin.hpp"
#include "sparsedouble_ctransposeBuiltin.hpp"
/*
#include "sparsedouble_plus_sparsedoubleBuiltin.hpp"
#include "sparsedouble_minus_sparsedoubleBuiltin.hpp"
#include "sparsedouble_eq_sparsedoubleBuiltin.hpp"
#include "sparselogical_doubleBuiltin.hpp"


#include "sparselogical_eq_sparselogicalBuiltin.hpp"
#include "sparsedouble_cosBuiltin.hpp"
#include "sparsedouble_sinBuiltin.hpp"
#include "sparsedouble_tanBuiltin.hpp"
#include "sparsedouble_coshBuiltin.hpp"
#include "sparsedouble_sinhBuiltin.hpp"
#include "sparsedouble_tanhBuiltin.hpp"
#include "sparsedouble_acosBuiltin.hpp"
#include "sparsedouble_asinBuiltin.hpp"
#include "sparsedouble_atanBuiltin.hpp"
*/
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"sparse";
//=============================================================================
static const nlsGateway gateway[] =
{
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
    { "sparselogical_vertcat_sparselogical", Nelson::SparseGateway::sparselogical_vertcat_sparselogicalBuiltin, 1, 2 },
    { "sparselogical_horzcat_sparselogical", Nelson::SparseGateway::sparselogical_horzcat_sparselogicalBuiltin, 1, 2 },
    { "sparsedouble_vertcat_sparsedouble", Nelson::SparseGateway::sparsedouble_vertcat_sparsedoubleBuiltin, 1, 2 },
    { "sparsedouble_horzcat_sparsedouble", Nelson::SparseGateway::sparsedouble_horzcat_sparsedoubleBuiltin, 1, 2 },
    { "sparselogical_ctranspose", Nelson::SparseGateway::sparselogical_ctransposeBuiltin, 1, 1 },
    { "sparsedouble_ctranspose", Nelson::SparseGateway::sparsedouble_ctransposeBuiltin, 1, 1 },

    /*
    { "sparselogical_double", sparselogical_doubleBuiltin, 0, 1);
    { "sparsedouble_plus_sparsedouble", sparsedouble_plus_sparsedoubleBuiltin, 0, 1);
    { "sparsedouble_minus_sparsedouble", sparsedouble_minus_sparsedoubleBuiltin, 0, 1);
    { "sparsedouble_eq_sparsedouble", sparsedouble_eq_sparsedoubleBuiltin, 2, 1);
    { "sparselogical_eq_sparselogical", sparselogical_eq_sparselogicalBuiltin, 2, 1);
    { "sparsedouble_cos", sparsedouble_cosBuiltin, 0, 1);
    { "sparsedouble_sin", sparsedouble_sinBuiltin, 0, 1);
    { "sparsedouble_tan", sparsedouble_tanBuiltin, 0, 1);
    { "sparsedouble_cosh", sparsedouble_coshBuiltin, 0, 1);
    { "sparsedouble_sinh", sparsedouble_sinhBuiltin, 0, 1);
    { "sparsedouble_tanh", sparsedouble_tanhBuiltin, 0, 1);
    { "sparsedouble_acos", sparsedouble_acosBuiltin, 0, 1);
    { "sparsedouble_asin", sparsedouble_asinBuiltin, 0, 1);
    { "sparsedouble_atan", sparsedouble_atanBuiltin, 0, 1);
    */
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
