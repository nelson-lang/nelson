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
[X, Y] = meshgrid(1:256, 1:256);
BW = mod(X, 13) < 4 | mod(Y, 19) < 5;
BW(72:184, 86:172) = true;
BW(106:150, 112:146) = false;
%=============================================================================
timerStart = tic();
for k = 1:12
  C = bwmorph(BW, 'clean');
  F = bwmorph(BW, 'fill');
  M = bwmorph(BW, 'majority');
  R = bwmorph(BW, 'remove');
  E = bwmorph(BW, 'endpoints');
  B = bwmorph(BW, 'branchpoints');
  D = bwmorph(BW, 'dilate');
  O = bwmorph(BW, 'open');
  H = bwmorph(BW, 'tophat');
end
elapsed = toc(timerStart);
ip_bench_report('bwmorph_local_ops_256_12_passes', elapsed, 2.0);
asserts.isequal(size(C), size(BW));
asserts.isequal(size(F), size(BW));
asserts.isequal(size(M), size(BW));
asserts.isequal(size(R), size(BW));
asserts.isequal(size(E), size(BW));
asserts.isequal(size(B), size(BW));
asserts.isequal(size(D), size(BW));
asserts.isequal(size(O), size(BW));
asserts.isequal(size(H), size(BW));
asserts.isequal(class(C), 'logical');
%=============================================================================
timerStart = tic();
for k = 1:8
  S = bwmorph(BW, 'spur', 4);
  G = bwmorph(BW, 'bridge', 2);
  D2 = bwmorph(BW, 'dilate', 2);
  E2 = bwmorph(BW, 'erode', 2);
  CL = bwmorph(BW, 'close', 2);
  BH = bwmorph(BW, 'bothat', 2);
  T = bwmorph(BW, 'thin', 4);
  K = bwmorph(BW, 'skel', 4);
end
elapsed = toc(timerStart);
ip_bench_report('bwmorph_iterative_ops_256_8_passes', elapsed, 3.0);
asserts.isequal(size(S), size(BW));
asserts.isequal(size(G), size(BW));
asserts.isequal(size(D2), size(BW));
asserts.isequal(size(E2), size(BW));
asserts.isequal(size(CL), size(BW));
asserts.isequal(size(BH), size(BW));
asserts.isequal(size(T), size(BW));
asserts.isequal(size(K), size(BW));
asserts.istrue(sum(T(:)) <= sum(BW(:)));
asserts.istrue(sum(K(:)) <= sum(BW(:)));
%=============================================================================
[X2, Y2] = meshgrid(1:128, 1:128);
BW2 = ((X2 - 64) .^ 2 + (Y2 - 64) .^ 2) <= 45 ^ 2;
BW2 = BW2 | (abs(X2 - Y2) < 4);
timerStart = tic();
THIN_INF = bwmorph(BW2, 'thin', Inf);
SKEL_INF = bwmorph(BW2, 'skel', Inf);
elapsed = toc(timerStart);
ip_bench_report('bwmorph_converge_thin_skel_128', elapsed, 2.0);
asserts.isequal(size(THIN_INF), size(BW2));
asserts.isequal(size(SKEL_INF), size(BW2));
asserts.isequal(class(THIN_INF), 'logical');
asserts.istrue(sum(THIN_INF(:)) <= sum(BW2(:)));
asserts.istrue(sum(SKEL_INF(:)) <= sum(BW2(:)));
clear X Y BW C F M R E B D O H S G D2 E2 CL BH T K k timerStart elapsed X2 Y2 BW2 THIN_INF SKEL_INF;
%=============================================================================
