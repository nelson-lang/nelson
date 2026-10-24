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
[X, Y, Z] = meshgrid(1:48, 1:48, 1:24);
V = single(((X - 24) .^ 2 + (Y - 22) .^ 2 + (2 * Z - 25) .^ 2) < 14 ^ 2);
tform = affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 2.25 -1.5 0.75 1]);
timerStart = tic();
W = imwarp(V, tform, 'linear', 'FillValues', 0);
elapsed = toc(timerStart);
ip_bench_report('imwarp3d_single48x48x24_linear', elapsed, 2.0);
asserts.isequal(size(W), size(V));
asserts.isequal(class(W), 'single');
clear X Y Z V tform W timerStart elapsed;
%=============================================================================
