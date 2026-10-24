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
BW = ((X - 18) .^ 2 + (Y - 18) .^ 2 + (Z - 10) .^ 2) < 8 ^ 2;
BW = BW | (((X - 46) .^ 2 + (Y - 42) .^ 2 + (Z - 22) .^ 2) < 7 ^ 2);
I = single(mod(X + 2 * Y + 3 * Z, 31));
timerStart = tic();
S = regionprops3(BW, I, 'Volume', 'Centroid', 'BoundingBox', 'MeanIntensity');
elapsed = toc(timerStart);
ip_bench_report('regionprops3_two_blobs_64x64x32', elapsed, 2.0);
asserts.istrue(length(S) >= 2);
asserts.istrue(isfield(S, 'MeanIntensity'));
%=============================================================================
