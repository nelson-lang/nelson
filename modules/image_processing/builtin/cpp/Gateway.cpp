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
#include "OverloadName.hpp"
#include "__activecontour_kernel__Builtin.hpp"
#include "__bwmorph_kernel__Builtin.hpp"
#include "__imgaussfilt3_kernel__Builtin.hpp"
#include "__imresize3_kernel__Builtin.hpp"
#include "__ip_regional_minima_2d__Builtin.hpp"
#include "__imwarp3d_kernel__Builtin.hpp"
#include "__imwarp_kernel__Builtin.hpp"
#include "__watershed_kernel__Builtin.hpp"
#include "bwconncompBuiltin.hpp"
#include "imfilterBuiltin.hpp"
#include "imgaussfiltBuiltin.hpp"
#include "imdilateBuiltin.hpp"
#include "imerodeBuiltin.hpp"
#include "imrotateBuiltin.hpp"
#include "imresizeBuiltin.hpp"
#include "medfilt2Builtin.hpp"
#include "regionpropsBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"image_processing";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__activecontour_kernel__",
        (ptrBuiltin)Nelson::ImageProcessingGateway::__activecontour_kernel__Builtin, 1, 6,
        CPP_BUILTIN },
    { "__bwmorph_kernel__", (ptrBuiltin)Nelson::ImageProcessingGateway::__bwmorph_kernel__Builtin,
        1, 3, CPP_BUILTIN },
    { "__imgaussfilt3_kernel__",
        (ptrBuiltin)Nelson::ImageProcessingGateway::__imgaussfilt3_kernel__Builtin, 1, 4,
        CPP_BUILTIN },
    { "__imresize3_kernel__",
        (ptrBuiltin)Nelson::ImageProcessingGateway::__imresize3_kernel__Builtin, 1, 3,
        CPP_BUILTIN },
    { "__imwarp_kernel__", (ptrBuiltin)Nelson::ImageProcessingGateway::__imwarp_kernel__Builtin, 1,
        9, CPP_BUILTIN },
    { "__imwarp3d_kernel__", (ptrBuiltin)Nelson::ImageProcessingGateway::__imwarp3d_kernel__Builtin,
        1, 9, CPP_BUILTIN },
    { "__ip_regional_minima_2d__",
        (ptrBuiltin)Nelson::ImageProcessingGateway::__ip_regional_minima_2d__Builtin, 1, 2,
        CPP_BUILTIN },
    { "__watershed_kernel__",
        (ptrBuiltin)Nelson::ImageProcessingGateway::__watershed_kernel__Builtin, 1, 2,
        CPP_BUILTIN },
    { "bwconncomp", (ptrBuiltin)Nelson::ImageProcessingGateway::bwconncompBuiltin, 1, -1,
        CPP_BUILTIN },
    { "imdilate", (ptrBuiltin)Nelson::ImageProcessingGateway::imdilateBuiltin, 1, -1, CPP_BUILTIN },
    { "imerode", (ptrBuiltin)Nelson::ImageProcessingGateway::imerodeBuiltin, 1, -1, CPP_BUILTIN },
    { "imfilter", (ptrBuiltin)Nelson::ImageProcessingGateway::imfilterBuiltin, 1, -1, CPP_BUILTIN },
    { "imgaussfilt", (ptrBuiltin)Nelson::ImageProcessingGateway::imgaussfiltBuiltin, 1, -1,
        CPP_BUILTIN },
    { "imrotate", (ptrBuiltin)Nelson::ImageProcessingGateway::imrotateBuiltin, 1, 2, CPP_BUILTIN },
    { "imresize", (ptrBuiltin)Nelson::ImageProcessingGateway::imresizeBuiltin, 1, 2, CPP_BUILTIN },
    { "medfilt2", (ptrBuiltin)Nelson::ImageProcessingGateway::medfilt2Builtin, 1, -1, CPP_BUILTIN },
    { "regionprops", (ptrBuiltin)Nelson::ImageProcessingGateway::regionpropsBuiltin, 1, -1,
        CPP_BUILTIN },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
