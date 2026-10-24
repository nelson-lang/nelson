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
V = V + single(mod(X + 2 * Y + 3 * Z, 19)) / 64;
timerStart = tic();
B = imresize3(V, [64 64 32], 'linear', 'Antialiasing', false);
elapsed = toc(timerStart);
ip_bench_report('imresize3_single48x48x24_to_64x64x32', elapsed, 2.0);
asserts.isequal(size(B), [64 64 32]);
asserts.isequal(class(B), 'single');
clear X Y Z V B timerStart elapsed;
%=============================================================================
