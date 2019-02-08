//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "h5writeBuiltin.hpp"
#include "h5writeattBuiltin.hpp"
#include "h5readattBuiltin.hpp"
#include "h5createBuiltin.hpp"
#include "h5readBuiltin.hpp"
#include "h5saveBuiltin.hpp"
#include "h5loadBuiltin.hpp"
#include "HDF5_helpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"hdf5";
//=============================================================================
static const nlsGateway gateway[] = {
    { "h5write", Nelson::Hdf5Gateway::h5writeBuiltin, 0, 3, CPP_BUILTIN },
    { "h5writeatt", Nelson::Hdf5Gateway::h5writeattBuiltin, 0, -1, CPP_BUILTIN },
    { "h5readatt", Nelson::Hdf5Gateway::h5readattBuiltin, 1, 3, CPP_BUILTIN },
    { "h5read", Nelson::Hdf5Gateway::h5readBuiltin, 1, 2, CPP_BUILTIN },
    { "h5create", Nelson::Hdf5Gateway::h5createBuiltin, 0, -4, CPP_BUILTIN },
    { "h5load", Nelson::Hdf5Gateway::h5loadBuiltin, 1, 1, CPP_BUILTIN },
    { "h5save", Nelson::Hdf5Gateway::h5saveBuiltin, 0, 1, CPP_BUILTIN },
    { "load", Nelson::Hdf5Gateway::h5loadBuiltin, 1, 1, CPP_BUILTIN },
    { "save", Nelson::Hdf5Gateway::h5saveBuiltin, 0, 1, CPP_BUILTIN },

};
//=============================================================================
static bool
initializeHdf5Module(Nelson::Evaluator* eval)
{
    disableHdf5Warning();
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeHdf5Module)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
