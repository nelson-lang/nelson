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
V = ((X - 18) .^ 2 + (Y - 18) .^ 2 + (Z - 10) .^ 2) < 8 ^ 2;
V = V | (((X - 46) .^ 2 + (Y - 42) .^ 2 + (Z - 22) .^ 2) < 7 ^ 2);
V = V | (mod(X, 13) == 0 & mod(Y, 11) == 0 & mod(Z, 7) == 0);
timerStart = tic();
CC = bwconncomp(V, 26);
elapsed = toc(timerStart);
ip_bench_report('bwconncomp3d_mixed_components_64x64x32', elapsed, 2.0);
asserts.istrue(CC.NumObjects >= 2);
asserts.isequal(CC.ImageSize, [64 64 32]);
asserts.isequal(CC.Connectivity, 26);
clear X Y Z V CC timerStart elapsed;
%=============================================================================
