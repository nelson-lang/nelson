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
[X, Y] = meshgrid(1:512, 1:512);
BW = ((X - 150) .^ 2 + (Y - 180) .^ 2 < 90 ^ 2) ...
  | ((X - 360) .^ 2 / 120 ^ 2 + (Y - 320) .^ 2 / 70 ^ 2 < 1) ...
  | (X > 80 & X < 210 & Y > 330 & Y < 470);
I = double(mod(7 * X + 11 * Y, 251));
CC = bwconncomp(BW, 8);
timerStart = tic();
for k = 1:20
  S = regionprops(CC, I, 'PixelIdxList', 'PixelList', 'PixelValues');
end
elapsed = toc(timerStart);
ip_bench_report('regionprops_pixel_copy_512_20_passes', elapsed, 2.0);
asserts.isequal(length(S), CC.NumObjects);
asserts.istrue(isfield(S, 'PixelValues'));
asserts.istrue(length(S(1).PixelIdxList) > 1000);
clear X Y BW I CC S k timerStart elapsed;
%=============================================================================
