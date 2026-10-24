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
I = rand(512, 512);
T = [1 0 0; 0 1 0; 18.5 11.25 1];
timerStart = tic();
J = imwarp(I, T, 'linear', 'OutputView', 'same');
elapsed = toc(timerStart);
ip_bench_report('imwarp_double512_linear_same', elapsed, 1.5);
RGB = zeros(256, 256, 3);
RGB(:, :, 1) = I(1:256, 1:256);
RGB(:, :, 2) = I(257:512, 1:256);
RGB(:, :, 3) = I(1:256, 257:512);
timerStart = tic();
JRGB = imwarp(RGB, T, 'cubic', 'FillValues', [0.1 0.2 0.3]);
elapsed = toc(timerStart);
ip_bench_report('imwarp_rgb256_cubic_fill', elapsed, 2.0);
R = imref2d([256 256], [10.5 266.5], [20.5 276.5]);
timerStart = tic();
[JW, RW] = imwarp(uint8(255 * RGB), R, T, 'nearest', 'OutputView', 'full');
elapsed = toc(timerStart);
ip_bench_report('imwarp_uint8_rgb256_ref_nearest_full', elapsed, 1.0);
clear I T J RGB JRGB R JW RW timerStart elapsed;
%=============================================================================
