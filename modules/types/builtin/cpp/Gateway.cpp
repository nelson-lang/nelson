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
#include "NelsonGateway.hpp"
#include "classBuiltin.hpp"
#include "isaBuiltin.hpp"
#include "iscellBuiltin.hpp"
#include "ischarBuiltin.hpp"
#include "isclassBuiltin.hpp"
#include "isdoubleBuiltin.hpp"
#include "isemptyBuiltin.hpp"
#include "isfloatBuiltin.hpp"
#include "ishandleBuiltin.hpp"
#include "isint16Builtin.hpp"
#include "isint32Builtin.hpp"
#include "isint64Builtin.hpp"
#include "isint8Builtin.hpp"
#include "isintegerBuiltin.hpp"
#include "islogicalBuiltin.hpp"
#include "isnumericBuiltin.hpp"
#include "isrealBuiltin.hpp"
#include "issingleBuiltin.hpp"
#include "issparseBuiltin.hpp"
#include "isstructBuiltin.hpp"
#include "isuint16Builtin.hpp"
#include "isuint32Builtin.hpp"
#include "isuint64Builtin.hpp"
#include "isuint8Builtin.hpp"
#include "isstringBuiltin.hpp"
#include "isvarnameBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"types";
//=============================================================================
static const nlsGateway gateway[] = {
    { "class", (void*)Nelson::TypeGateway::classBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ischar", (void*)Nelson::TypeGateway::ischarBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isstring", (void*)Nelson::TypeGateway::isstringBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isfloat", (void*)Nelson::TypeGateway::isfloatBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "iscell", (void*)Nelson::TypeGateway::iscellBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isdouble", (void*)Nelson::TypeGateway::isdoubleBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "issingle", (void*)Nelson::TypeGateway::issingleBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isstruct", (void*)Nelson::TypeGateway::isstructBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isinteger", (void*)Nelson::TypeGateway::isintegerBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isint8", (void*)Nelson::TypeGateway::isint8Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isint16", (void*)Nelson::TypeGateway::isint16Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isint32", (void*)Nelson::TypeGateway::isint32Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isint64", (void*)Nelson::TypeGateway::isint64Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isuint8", (void*)Nelson::TypeGateway::isuint8Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isuint16", (void*)Nelson::TypeGateway::isuint16Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isuint32", (void*)Nelson::TypeGateway::isuint32Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isuint64", (void*)Nelson::TypeGateway::isuint64Builtin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "issparse", (void*)Nelson::TypeGateway::issparseBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "islogical", (void*)Nelson::TypeGateway::islogicalBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isnumeric", (void*)Nelson::TypeGateway::isnumericBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isa", (void*)Nelson::TypeGateway::isaBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "isreal", (void*)Nelson::TypeGateway::isrealBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isempty", (void*)Nelson::TypeGateway::isemptyBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isclass", (void*)Nelson::TypeGateway::isclassBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "ishandle", (void*)Nelson::TypeGateway::ishandleBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isvarname", (void*)Nelson::TypeGateway::isvarnameBuiltin, 1, 1, CPP_BUILTIN },
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
