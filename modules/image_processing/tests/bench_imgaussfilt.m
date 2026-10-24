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
A = rand(512, 512);
timerStart = tic();
B = imgaussfilt(A, 1.5);
elapsed = toc(timerStart);
ip_bench_report('imgaussfilt_double512_sigma15', elapsed, 1.0);
RGB = zeros(256, 256, 3);
RGB(:, :, 1) = A(1:256, 1:256);
RGB(:, :, 2) = A(257:512, 1:256);
RGB(:, :, 3) = A(1:256, 257:512);
timerStart = tic();
BRGB = imgaussfilt(RGB, [1.2 1.8], 'FilterSize', [7 11], 'Padding', 'symmetric');
elapsed = toc(timerStart);
ip_bench_report('imgaussfilt_rgb256_symmetric', elapsed, 1.0);
U = uint8(255 * A(1:256, 1:256));
timerStart = tic();
BU = imgaussfilt(U, 1.0, 'FilterSize', [5 5]);
elapsed = toc(timerStart);
ip_bench_report('imgaussfilt_uint8_256', elapsed, 1.0);
%=============================================================================
