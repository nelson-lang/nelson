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
V = ((X - 24) .^ 2 + (Y - 24) .^ 2 + (Z - 12) .^ 2) < 7 ^ 2;
V = V | (((X - 44) .^ 2 + (Y - 42) .^ 2 + (Z - 22) .^ 2) < 6 ^ 2);
SE = strel('sphere', 2);
timerStart = tic();
D = imdilate(V, SE);
E = imerode(D, SE);
elapsed = toc(timerStart);
ip_bench_report('morphology3d_sphere64x64x32', elapsed, 2.0);
asserts.isequal(size(D), size(V));
asserts.isequal(size(E), size(V));
asserts.istrue(sum(D(:)) >= sum(V(:)));
clear X Y Z V SE D E timerStart elapsed;
%=============================================================================
