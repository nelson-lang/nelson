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
[X, Y, Z] = meshgrid(1:64, 1:64, 1:32);
V = single(((X - 32) .^ 2 + (Y - 30) .^ 2 + (2 * Z - 35) .^ 2) < 18 ^ 2);
V = V + single(mod(X + 2 * Y + 3 * Z, 17)) / 64;
timerStart = tic();
B = imgaussfilt3(V, [1.0 1.5 0.75], 'FilterSize', [5 7 5], 'Padding', 'replicate');
elapsed = toc(timerStart);
ip_bench_report('imgaussfilt3_single64x64x32_replicate', elapsed, 2.0);
asserts.isequal(size(B), size(V));
asserts.isequal(class(B), 'single');
clear X Y Z V B timerStart elapsed;
%=============================================================================
