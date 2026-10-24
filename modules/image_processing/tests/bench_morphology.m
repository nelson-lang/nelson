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
[X, Y] = meshgrid(1:160, 1:160);
BW = mod(X, 17) < 5 | mod(Y, 23) < 4;
BW(50:110, 55:120) = true;
GRAY = double(BW) + 0.25 * sin(X / 11) + 0.1 * cos(Y / 7);
SE1 = strel('square', 5);
SE2 = strel('disk', 3);
timerStart = tic();
for k = 1:4
  D = imdilate(BW, SE1);
  E = imerode(D, SE1);
  O = imopen(GRAY, SE2);
  C = imclose(GRAY, SE2);
end
elapsed = toc(timerStart);
ip_bench_report('morphology2d_binary_gray_160_four_passes', elapsed, 2.0);
asserts.isequal(size(D), size(BW));
asserts.isequal(size(E), size(BW));
asserts.isequal(size(O), size(GRAY));
asserts.isequal(size(C), size(GRAY));
clear X Y BW GRAY SE1 SE2 D E O C k timerStart elapsed;
%=============================================================================
