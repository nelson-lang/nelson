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
BW = false(512, 512);
BW(4:8:end, 4:8:end) = true;
BW(5:8:end, 4:8:end) = true;
BW(4:8:end, 5:8:end) = true;
timerStart = tic();
for k = 1:12
  CC4 = bwconncomp(BW, 4);
  CC8 = bwconncomp(BW, 8);
end
elapsed = toc(timerStart);
ip_bench_report('bwconncomp_many_small_components_512', elapsed, 2.0);
asserts.istrue(CC4.NumObjects > 3000);
asserts.isequal(CC4.NumObjects, CC8.NumObjects);
asserts.isequal(length(CC8.PixelIdxList), CC8.NumObjects);
clear BW CC4 CC8 k timerStart elapsed;
%=============================================================================
