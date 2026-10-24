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
BW = mod(X, 11) < 3 | mod(Y, 17) < 4;
BW(80:170, 90:180) = true;
BW(110:130, 115:145) = false;
timerStart = tic();
for k = 1:3
  C = bwmorph(BW, 'clean');
  F = bwmorph(BW, 'fill');
  R = bwmorph(BW, 'remove');
  E = bwmorph(BW, 'endpoints');
  S = bwmorph(BW, 'spur', 3);
  T = bwmorph(BW, 'thin', 3);
  K = bwmorph(BW, 'skel', 3);
end
I = mod(X, 37) + mod(Y, 29) + 0.1 * sin(X / 13) + 0.2 * cos(Y / 17);
I(45:95, 45:115) = 2;
I(145:205, 130:215) = 1;
L4 = watershed(I, 4);
L8 = watershed(I, 8);
MASK = false(size(I));
MASK(120:140, 120:140) = true;
AC = activecontour(I <= 2, MASK, 12);
ACRGB = activecontour(cat(3, I <= 2, I <= 2, I <= 2), MASK, 12);
elapsed = toc(timerStart);
ip_bench_report('bwmorph_watershed_activecontour_256', elapsed, 4.0);
asserts.isequal(size(C), size(BW));
asserts.isequal(size(F), size(BW));
asserts.isequal(size(R), size(BW));
asserts.isequal(size(E), size(BW));
asserts.isequal(size(S), size(BW));
asserts.isequal(size(T), size(BW));
asserts.isequal(size(K), size(BW));
asserts.isequal(size(L4), size(I));
asserts.isequal(size(L8), size(I));
asserts.isequal(size(AC), size(I));
asserts.isequal(size(ACRGB), size(I));
asserts.isequal(class(S), 'logical');
asserts.isequal(class(T), 'logical');
asserts.isequal(class(K), 'logical');
asserts.isequal(class(L4), 'double');
asserts.isequal(class(AC), 'logical');
asserts.isequal(class(ACRGB), 'logical');
asserts.istrue(all(L4(:) >= 0));
asserts.istrue(all(L8(:) >= 0));
timerStart = tic();
[X2, Y2] = meshgrid(1:64, 1:64);
I2 = 10 + 3 * sin(X2 / 9) + 2 * cos(Y2 / 11);
I2(12:22, 14:26) = 1;
I2(42:54, 38:50) = 2;
I2(26:36, 28:36) = 18;
I2(48:58, 10:20) = 16;
MIN2 = imregionalmin(I2, 8);
MAX2 = imregionalmax(I2, 8);
HMIN2 = imhmin(I2, 2, 8);
HMAX2 = imhmax(I2, 2, 8);
EMIN2 = imextendedmin(I2, 2, 8);
EMAX2 = imextendedmax(I2, 2, 8);
MARK2 = EMIN2;
IMPOSED2 = imimposemin(I2, MARK2, 8);
L_MARK2 = watershed(IMPOSED2, 8);
elapsed = toc(timerStart);
ip_bench_report('marker_extrema_watershed_64', elapsed, 3.0);
asserts.isequal(size(MIN2), size(I2));
asserts.isequal(size(MAX2), size(I2));
asserts.isequal(size(HMIN2), size(I2));
asserts.isequal(size(HMAX2), size(I2));
asserts.isequal(size(EMIN2), size(I2));
asserts.isequal(size(EMAX2), size(I2));
asserts.isequal(size(IMPOSED2), size(I2));
asserts.isequal(size(L_MARK2), size(I2));
asserts.isequal(class(MIN2), 'logical');
asserts.isequal(class(MAX2), 'logical');
asserts.isequal(class(EMIN2), 'logical');
asserts.isequal(class(EMAX2), 'logical');
asserts.isequal(class(L_MARK2), 'double');
asserts.istrue(any(MIN2(:)));
asserts.istrue(any(MAX2(:)));
asserts.istrue(any(EMIN2(:)));
asserts.istrue(any(EMAX2(:)));
asserts.istrue(all(L_MARK2(:) >= 0));
timerStart = tic();
[X3, Y3, Z3] = ndgrid(1:32, 1:32, 1:16);
V3 = min((X3 - 10) .^ 2 + (Y3 - 12) .^ 2 + (Z3 - 6) .^ 2, ...
  (X3 - 22) .^ 2 + (Y3 - 20) .^ 2 + (Z3 - 11) .^ 2);
L3_6 = watershed(V3, 6);
L3_26 = watershed(V3, 26);
elapsed = toc(timerStart);
ip_bench_report('watershed3d_32x32x16', elapsed, 2.0);
asserts.isequal(size(L3_6), size(V3));
asserts.isequal(size(L3_26), size(V3));
asserts.isequal(class(L3_6), 'double');
asserts.istrue(all(L3_6(:) >= 0));
asserts.istrue(all(L3_26(:) >= 0));
clear X Y BW C F R E S T K I L4 L8 MASK AC ACRGB k timerStart elapsed;
clear X2 Y2 I2 MIN2 MAX2 HMIN2 HMAX2 EMIN2 EMAX2 MARK2 IMPOSED2 L_MARK2;
clear X3 Y3 Z3 V3 L3_6 L3_26;
%=============================================================================
