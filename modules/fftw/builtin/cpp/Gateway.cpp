//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "fftBuiltin.hpp"
#include "fftwBuiltin.hpp"
#include "ifftBuiltin.hpp"
#include "FFTWwrapperBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"fftw";
//=============================================================================
static const nlsGateway gateway[] = {
    { "fft", (ptrBuiltin)Nelson::FftwGateway::fftBuiltin, 1, 3 },
    { "ifft", (ptrBuiltin)Nelson::FftwGateway::ifftBuiltin, 1, 3 },
    { "fftw", (ptrBuiltin)Nelson::FftwGateway::fftwBuiltin, 1, 2 },
    { "FFTWwrapper", (ptrBuiltin)Nelson::FftwGateway::FFTWwrapperBuiltin, 1, 3 },
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
