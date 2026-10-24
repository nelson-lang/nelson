%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = [0 0; 1 0; 1 1; 0 1];
shp = alphaShape(p);
asserts.isapprox(area(shp), 1, 1e-12);
asserts.isequal(numRegions(shp), 1);
asserts.istrue(inShape(shp, [0.25 0.25]));
%=============================================================================
