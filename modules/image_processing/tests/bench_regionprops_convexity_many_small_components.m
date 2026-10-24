%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
BW = false(256, 256);
for r = 2:8:250
  for c = 2:8:250
    BW(r, c) = true;
    BW(r + 1, c) = true;
    BW(r, c + 1) = true;
  end
end
CC = bwconncomp(BW, 8);
timerStart = tic();
for k = 1:3
  S = regionprops(CC, 'ConvexHull', 'ConvexImage', 'ConvexArea', 'Solidity');
end
elapsed = toc(timerStart);
ip_bench_report('regionprops_convexity_many_small_256', elapsed, 2.0);
asserts.isequal(length(S), CC.NumObjects);
asserts.istrue(CC.NumObjects > 500);
asserts.isequal(S(1).ConvexImage, true(2, 2));
asserts.isequal(S(1).ConvexArea, 4);
asserts.isapprox(S(1).Solidity, 0.75, 1e-15);
clear BW CC S r c k timerStart elapsed;
%=============================================================================
