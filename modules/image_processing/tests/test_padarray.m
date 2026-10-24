%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
A = [1 2; 3 4];
asserts.isequal(padarray(A, [1 1], 9, 'pre'), [9 9 9; 9 1 2; 9 3 4]);
asserts.isequal(padarray(A, [1 1], 8, 'post'), [1 2 8; 3 4 8; 8 8 8]);
asserts.isequal(padarray(A, [1 1], 'circular'), [4 3 4 3; 2 1 2 1; 4 3 4 3; 2 1 2 1]);
asserts.isequal(padarray(A, [1 1], 'symmetric'), [1 1 2 2; 1 1 2 2; 3 3 4 4; 3 3 4 4]);
%=============================================================================
A = [1 2; 3 4];
P = padarray(A, [1 1], 'replicate');
REF = [1 1 2 2; 1 1 2 2; 3 3 4 4; 3 3 4 4];
asserts.isequal(P, REF);
asserts.isequal(padarray(A, [1 1], 'Replicate'), REF);
asserts.isequal(class(padarray(int16(A), [1 1], 0)), 'int16');
asserts.isequal(class(padarray(uint32(A), [1 1], 0)), 'uint32');
asserts.isequal(class(padarray(int32(A), [1 1], 0)), 'int32');
asserts.isequal(class(padarray(uint64(A), [1 1], 0)), 'uint64');
asserts.isequal(class(padarray(int64(A), [1 1], 0)), 'int64');
RGB32 = uint32(reshape(1:12, [2 2 3]));
asserts.isequal(class(padarray(RGB32, [1 1], 0)), 'uint32');
H = fspecial('average', [3 3]);
asserts.isapprox(sum(H(:)), 1, 1e-15);
HD = fspecial('disk', 1);
asserts.isapprox(HD, [0 1 0; 1 1 1; 0 1 0] / 5, 1e-15);
F = imfilter(A, H, 'replicate');
asserts.isequal(size(F), size(A));
asserts.isequal(imfilter(A, ones(1, 1), 'Replicate'), A);
asserts.isapprox(imfilter(A, ones(2, 2), 0, 'full'), conv2(A, rot90(ones(2, 2), 2), 'full'), 1e-15);
asserts.isapprox(imfilter(A, ones(2, 2), 0, 'valid'), conv2(A, rot90(ones(2, 2), 2), 'valid'), 1e-15);
FI = imfilter(int16(A), ones(1, 1), 0);
asserts.isequal(class(FI), 'int16');
asserts.isequal(FI, int16(A));
FU32 = imfilter(uint32(A), ones(1, 1), 0);
asserts.isequal(class(FU32), 'double');
asserts.isequal(FU32, double(A));
FC = imfilter([1 + i 2; 3 4], ones(1, 1), 0);
asserts.isequal(class(FC), 'double');
asserts.isequal(FC, [1 + i 2; 3 4]);
RGB = zeros(2, 2, 3);
RGB(:, :, 1) = A;
RGB(:, :, 2) = 10 * A;
RGB(:, :, 3) = 100 * A;
FRGB = imfilter(RGB, ones(1, 1), 0);
asserts.isequal(size(FRGB), [2 2 3]);
asserts.isequal(FRGB(:, :, 1), A);
asserts.isequal(FRGB(:, :, 2), 10 * A);
asserts.isequal(FRGB(:, :, 3), 100 * A);
B = imboxfilt(A, [3 3]);
asserts.isapprox(F, B, 1e-15);
SUM_BOX = imboxfilt(A, [2 2], 'Padding', 0, 'NormalizationFactor', 1);
asserts.isapprox(SUM_BOX, imfilter(A, ones(2, 2), 0), 1e-15);
K = [0 0 0; 0 0 0; 0 0 1];
CORR = imfilter(A, K, 0);
CONV = imfilter(A, K, 0, 'conv');
asserts.isequal(CORR, [4 0; 0 0]);
asserts.isequal(CONV, [0 0; 0 1]);
G = imgaussfilt(A, 0.5);
asserts.isequal(size(G), size(A));
G0 = imgaussfilt(A, 0.5, 'FilterSize', [3 3], 'Padding', 0, 'FilterDomain', 'spatial');
asserts.isapprox(G0, imfilter(A, fspecial('gaussian', [3 3], 0.5), 0), 1e-15);
GS = imgaussfilt(single(A), [0.5 1.0], 'FilterSize', [3 5], 'Padding', 'replicate');
asserts.isequal(class(GS), 'single');
asserts.isequal(size(GS), size(A));
GS_CASE = imgaussfilt(single(A), [0.5 1.0], 'filtersize', [3 5], ...
  'padding', 'Replicate', 'filterdomain', 'Frequency');
asserts.isequal(GS_CASE, GS);
GRGB = imgaussfilt(RGB, 0.5, 'FilterSize', [3 3], 'Padding', 0);
asserts.isequal(size(GRGB), [2 2 3]);
asserts.isapprox(GRGB(:, :, 1), imgaussfilt(A, 0.5, 'FilterSize', [3 3], 'Padding', 0), 1e-15);
asserts.isapprox(GRGB(:, :, 2), 10 * imgaussfilt(A, 0.5, 'FilterSize', [3 3], 'Padding', 0), 1e-14);
asserts.isapprox(GRGB(:, :, 3), 100 * imgaussfilt(A, 0.5, 'FilterSize', [3 3], 'Padding', 0), 1e-13);
RGBA = cat(3, RGB, 0.25 * A);
GRGBA = imgaussfilt(RGBA, 0.5, 'FilterSize', [3 3], 'Padding', 0);
asserts.isequal(size(GRGBA), [2 2 4]);
asserts.isapprox(GRGBA(:, :, 4), 0.25 * imgaussfilt(A, 0.5, 'FilterSize', [3 3], 'Padding', 0), 1e-15);
GRGBA_U8 = imgaussfilt(uint8(10 * RGBA), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GRGBA_U8), 'uint8');
asserts.isequal(GRGBA_U8(:, :, 4), uint8(10 * RGBA(:, :, 4)));
HG = fspecial('gaussian', [3 5], 0.75);
GCIRC = imgaussfilt(A, 0.75, 'FilterSize', [3 5], 'Padding', 'circular');
asserts.isapprox(GCIRC, imfilter(A, HG, 'circular'), 1e-14);
GSYM = imgaussfilt(A, 0.75, 'FilterSize', [3 5], 'Padding', 'symmetric');
asserts.isapprox(GSYM, imfilter(A, HG, 'symmetric'), 1e-14);
GU8 = imgaussfilt(uint8(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GU8), 'uint8');
asserts.isequal(GU8, uint8(A));
GU16 = imgaussfilt(uint16(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GU16), 'uint16');
asserts.isequal(GU16, uint16(A));
GI16 = imgaussfilt(int16(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GI16), 'int16');
asserts.isequal(GI16, int16(A));
GI8 = imgaussfilt(int8(A) - 3, 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GI8), 'int8');
asserts.isequal(GI8, int8(A) - 3);
GU32 = imgaussfilt(uint32(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GU32), 'uint32');
asserts.isequal(GU32, uint32(A));
GU64 = imgaussfilt(uint64(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GU64), 'uint64');
asserts.isequal(GU64, uint64(A));
GI32 = imgaussfilt(int32(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GI32), 'int32');
asserts.isequal(GI32, int32(A));
GI64 = imgaussfilt(int64(A), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GI64), 'int64');
asserts.isequal(GI64, int64(A));
GL = imgaussfilt(logical(A > 2), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GL), 'logical');
asserts.isequal(GL, logical(A > 2));
GLOGICAL_FILL = imgaussfilt(logical(A > 2), 0.5, 'FilterSize', [3 3], 'Padding', true);
asserts.isequal(class(GLOGICAL_FILL), 'logical');
asserts.isequal(size(GLOGICAL_FILL), size(A));
GNAN = imgaussfilt([1 2 3; 4 NaN 6; 7 8 9], 0.5, 'FilterSize', [3 3]);
asserts.istrue(isnan(GNAN(2, 2)));
GINF = imgaussfilt([1 2 3; 4 Inf 6; 7 8 9], 0.5, 'FilterSize', [3 3]);
asserts.istrue(isinf(GINF(2, 2)));
GC = imgaussfilt([1 + i 2; 3 4 - 2i], 0.5, 'FilterSize', [1 1]);
asserts.isequal(GC, [1 + i 2; 3 4 - 2i]);
GCS = imgaussfilt(single([1 + i 2; 3 4 - 2i]), 0.5, 'FilterSize', [1 1]);
asserts.isequal(class(GCS), 'single');
asserts.isequal(GCS, single([1 + i 2; 3 4 - 2i]));
V3 = reshape(1:8, [2 2 2]);
G3_ID = imgaussfilt3(V3, 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(G3_ID, V3);
IMPULSE = zeros(3, 3, 3);
IMPULSE(2, 2, 2) = 1;
K1 = exp(-((-1:1) .^ 2) / (2 * 0.5 ^ 2));
K1 = K1 / sum(K1);
REF3 = zeros(3, 3, 3);
for rr = 1:3
  for cc = 1:3
    for pp = 1:3
      REF3(rr, cc, pp) = K1(rr) * K1(cc) * K1(pp);
    end
  end
end
G3 = imgaussfilt3(IMPULSE, 0.5, 'FilterSize', [3 3 3], 'Padding', 0);
asserts.isapprox(G3, REF3, 1e-15);
G3_SINGLE = imgaussfilt3(single(V3), [0.5 0.75 1], 'FilterSize', [3 3 5], ...
  'Padding', 'replicate');
asserts.isequal(class(G3_SINGLE), 'single');
asserts.isequal(size(G3_SINGLE), [2 2 2]);
G3_UINT8 = imgaussfilt3(uint8(V3), 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(class(G3_UINT8), 'uint8');
asserts.isequal(G3_UINT8, uint8(V3));
G3_INT8 = imgaussfilt3(int8(V3) - 4, 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(class(G3_INT8), 'int8');
asserts.isequal(G3_INT8, int8(V3) - 4);
G3_UINT32 = imgaussfilt3(uint32(V3), 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(class(G3_UINT32), 'uint32');
asserts.isequal(G3_UINT32, uint32(V3));
G3_UINT64 = imgaussfilt3(uint64(V3), 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(class(G3_UINT64), 'uint64');
asserts.isequal(G3_UINT64, uint64(V3));
G3_INT64 = imgaussfilt3(int64(V3) - 4, 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(class(G3_INT64), 'int64');
asserts.isequal(G3_INT64, int64(V3) - 4);
G3_LOGICAL = imgaussfilt3(logical(V3 > 4), 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(class(G3_LOGICAL), 'logical');
asserts.isequal(G3_LOGICAL, logical(V3 > 4));
G3_LOGICAL_PADDING = imgaussfilt3(false(2, 2, 2), 0.5, 'FilterSize', [3 3 3], ...
  'Padding', true);
asserts.isequal(class(G3_LOGICAL_PADDING), 'logical');
asserts.isequal(size(G3_LOGICAL_PADDING), [2 2 2]);
G3_NAN = imgaussfilt3(cat(3, [1 2; 3 4], [5 6; 7 NaN]), 0.5, 'FilterSize', [3 3 3]);
asserts.istrue(isnan(G3_NAN(2, 2, 2)));
G3_INF = imgaussfilt3(cat(3, [1 2; 3 4], [5 6; 7 Inf]), 0.5, 'FilterSize', [3 3 3]);
asserts.istrue(isinf(G3_INF(2, 2, 2)));
G3_COMPLEX = cat(3, [1 + i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
G3_COMPLEX_ID = imgaussfilt3(G3_COMPLEX, 0.5, 'FilterSize', [1 1 1]);
asserts.isequal(G3_COMPLEX_ID, G3_COMPLEX);
G3_COMPLEX_FILTERED = imgaussfilt3(G3_COMPLEX, 0.5, 'FilterSize', [3 3 3], 'Padding', 0);
asserts.isapprox(real(G3_COMPLEX_FILTERED), imgaussfilt3(real(G3_COMPLEX), 0.5, ...
  'FilterSize', [3 3 3], 'Padding', 0), 1e-14);
asserts.isapprox(imag(G3_COMPLEX_FILTERED), imgaussfilt3(imag(G3_COMPLEX), 0.5, ...
  'FilterSize', [3 3 3], 'Padding', 0), 1e-14);
M = medfilt2([1 100 2; 3 4 5; 6 7 8], [3 3]);
asserts.isequal(M, [0 2 0; 3 5 4; 0 4 0]);
MS = medfilt2([1 100 2; 3 4 5; 6 7 8], [3 3], 'symmetric');
asserts.isequal(MS, [3 3 4; 4 5 5; 6 6 7]);
asserts.isequal(medfilt2([1 100 2; 3 4 5; 6 7 8], [3 3], 'Symmetric'), MS);
MU8 = medfilt2(uint8([1 100 2; 3 4 5; 6 7 8]), [3 3], 'indexed');
asserts.isequal(class(MU8), 'uint8');
asserts.isequal(MU8, uint8([0 2 0; 3 5 4; 0 4 0]));
M_INT16 = medfilt2(int16(A), [1 1]);
asserts.isequal(class(M_INT16), 'int16');
asserts.isequal(M_INT16, int16(A));
M_UINT32 = medfilt2(uint32(A), [1 1]);
asserts.isequal(class(M_UINT32), 'uint32');
asserts.isequal(M_UINT32, uint32(A));
M_UINT64 = medfilt2(uint64(A), [1 1]);
asserts.isequal(class(M_UINT64), 'uint64');
asserts.isequal(M_UINT64, uint64(A));
M_INT32 = medfilt2(int32(A), [1 1]);
asserts.isequal(class(M_INT32), 'int32');
asserts.isequal(M_INT32, int32(A));
M_INT64 = medfilt2(int64(A), [1 1]);
asserts.isequal(class(M_INT64), 'int64');
asserts.isequal(M_INT64, int64(A));
MSINGLE = medfilt2(single([1 100 2; 3 4 5; 6 7 8]), [3 3], 'replicate');
asserts.isequal(class(MSINGLE), 'single');
asserts.isequal(MSINGLE, single([3 3 4; 4 5 5; 6 6 7]));
MLOGICAL = medfilt2(logical([1 0 1; 0 1 0; 1 0 1]), [3 3], 'symmetric');
asserts.isequal(class(MLOGICAL), 'logical');
asserts.isequal(MLOGICAL, logical([1 1 1; 1 1 1; 1 1 1]));
BW = edge([0 0 0; 0 1 1; 0 1 1], 'sobel');
asserts.istrue(islogical(BW));
[BW2, THRESH] = edge([0 0 0; 0 1 1; 0 1 1], 'sobel');
asserts.istrue(islogical(BW2));
asserts.istrue(THRESH >= 0);
[~, THRESH2] = edge([0 0 0; 0 1 1; 0 1 1], 'sobel', 0.25);
asserts.isequal(THRESH2, 0.25);
I = [0 0 0; 0 1 1; 0 1 1];
[BW_PREWITT, THRESH_PREWITT] = edge(I, 'prewitt', 0.25);
asserts.isequal(BW_PREWITT, logical([1 1 1; 1 1 1; 1 1 0]));
asserts.isequal(THRESH_PREWITT, 0.25);
[BW_ROBERTS, THRESH_ROBERTS] = edge(I, 'roberts', 0.25);
asserts.isequal(BW_ROBERTS, logical([0 0 0; 0 1 1; 0 1 0]));
asserts.isequal(THRESH_ROBERTS, 0.25);
[BW_SOBEL_HORIZONTAL, THRESH_SOBEL_HORIZONTAL] = edge(I, 'sobel', 0.25, 'horizontal');
asserts.isequal(BW_SOBEL_HORIZONTAL, logical([1 1 1; 1 1 1; 0 0 0]));
asserts.isequal(THRESH_SOBEL_HORIZONTAL, 0.25);
[BW_SOBEL_VERTICAL, THRESH_SOBEL_VERTICAL] = edge(I, 'sobel', 0.25, 'vertical');
asserts.isequal(BW_SOBEL_VERTICAL, logical([1 1 0; 1 1 0; 1 1 0]));
asserts.isequal(THRESH_SOBEL_VERTICAL, 0.25);
[BW_PREWITT_BOTH, THRESH_PREWITT_BOTH] = edge(I, 'prewitt', 0.25, 'both');
asserts.isequal(BW_PREWITT_BOTH, BW_PREWITT);
asserts.isequal(THRESH_PREWITT_BOTH, 0.25);
[BW_ROBERTS_VERTICAL, THRESH_ROBERTS_VERTICAL] = edge(I, 'roberts', 0.25, 'vertical');
asserts.isequal(BW_ROBERTS_VERTICAL, logical([0 0 0; 0 0 1; 0 1 0]));
asserts.isequal(THRESH_ROBERTS_VERTICAL, 0.25);
[BW_LOG, THRESH_LOG] = edge(I, 'log', 0.25);
asserts.isequal(BW_LOG, logical([0 0 0; 0 0 0; 0 0 0]));
asserts.isequal(THRESH_LOG, 0.25);
[BW_CANNY, THRESH_CANNY] = edge(I, 'canny', 0.25);
asserts.isequal(BW_CANNY, logical([0 0 0; 0 1 0; 0 0 0]));
asserts.isequal(THRESH_CANNY, [0.1 0.25]);
[BW_CANNY_SIGMA, THRESH_CANNY_SIGMA] = edge(I, 'Canny', [0.1 0.25], 1.0);
asserts.isequal(BW_CANNY_SIGMA, BW_CANNY);
asserts.isequal(THRESH_CANNY_SIGMA, [0.1 0.25]);
RGB_EDGE = cat(3, I, 0.5 * I, 0.25 * I);
GRAY_EDGE = rgb2gray(RGB_EDGE);
[BW_RGB_CANNY, THRESH_RGB_CANNY] = edge(RGB_EDGE, 'canny', [0.05 0.2], 1.0);
[BW_GRAY_CANNY, THRESH_GRAY_CANNY] = edge(GRAY_EDGE, 'canny', [0.05 0.2], 1.0);
asserts.isequal(BW_RGB_CANNY, BW_GRAY_CANNY);
asserts.isequal(THRESH_RGB_CANNY, THRESH_GRAY_CANNY);
CANNY_INTERP_INPUT = zeros(7, 7);
CANNY_INTERP_INPUT(3:5, 3:5) = [0.2 0.7 1.0; 0.4 1.0 0.8; 0.1 0.6 0.9];
[BW_CANNY_INTERP, THRESH_CANNY_INTERP] = edge(CANNY_INTERP_INPUT, 'canny', [0.05 0.15], 1.0);
CANNY_INTERP_REF = logical([0 0 0 0 0 0 0; 0 0 0 1 0 0 0; 0 0 1 1 1 1 0; ...
  0 0 1 0 0 1 0; 0 0 1 1 1 1 0; 0 0 0 0 0 0 0; 0 0 0 0 0 0 0]);
asserts.isequal(BW_CANNY_INTERP, CANNY_INTERP_REF);
asserts.isequal(THRESH_CANNY_INTERP, [0.05 0.15]);
CANNY_CHAIN_INPUT = zeros(7, 9);
CANNY_CHAIN_INPUT(4:7, 2:8) = 1;
CANNY_CHAIN_INPUT(2, 2) = 0.8;
[BW_CANNY_CHAIN, THRESH_CANNY_CHAIN] = edge(CANNY_CHAIN_INPUT, 'canny', [0.05 0.3], 1.0);
CANNY_CHAIN_REF = logical([0 0 0 0 0 0 0 0 0; 0 0 0 0 0 0 0 0 0; ...
  0 0 0 0 0 0 0 0 0; 0 1 1 1 1 1 1 1 0; 0 1 0 0 0 0 0 1 0; ...
  0 1 0 0 0 0 0 1 0; 0 0 0 0 0 0 0 0 0]);
asserts.isequal(BW_CANNY_CHAIN, CANNY_CHAIN_REF);
asserts.isequal(THRESH_CANNY_CHAIN, [0.05 0.3]);
[BW_CANNY_FLAT, THRESH_CANNY_FLAT] = edge(zeros(5, 5), 'canny');
asserts.isequal(BW_CANNY_FLAT, false(5, 5));
asserts.isequal(THRESH_CANNY_FLAT, [0 0]);
[BW_CANNY_ZERO, THRESH_CANNY_ZERO] = edge(zeros(5, 5), 'canny', 0);
asserts.isequal(BW_CANNY_ZERO, false(5, 5));
asserts.isequal(THRESH_CANNY_ZERO, [0 0]);
[BW_LOG_SIGMA, THRESH_LOG_SIGMA] = edge(I, 'LoG', 0.25, 1.0);
asserts.isequal(BW_LOG_SIGMA, logical([0 0 0; 0 0 0; 0 0 0]));
asserts.isequal(THRESH_LOG_SIGMA, 0.25);
[BW_RGB_LOG, THRESH_RGB_LOG] = edge(RGB_EDGE, 'log', 0.1, 1.0);
[BW_GRAY_LOG, THRESH_GRAY_LOG] = edge(GRAY_EDGE, 'log', 0.1, 1.0);
asserts.isequal(BW_RGB_LOG, BW_GRAY_LOG);
asserts.isequal(THRESH_RGB_LOG, THRESH_GRAY_LOG);
asserts.checkerror('edge(I, ''canny'', [0.1 0.2 0.3])', _('Threshold must be a scalar or a 2-element vector.'));
asserts.checkerror('edge(I, ''sobel'', 0.25, ''diagonal'')', sprintf(_('Unsupported "%s" option.'), 'edge direction'));
asserts.checkerror('edge(I, ''sobel'', 0.25, 1)', sprintf(_('%s must be a string.'), 'Edge direction'));
asserts.checkerror('edge(I, ''log'', 0.25, 0)', sprintf(_('%s must be a positive finite scalar.'), 'Sigma'));
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'padarray') && fixtures.available.padarray
  asserts.isequal(padarray(A, [1 1], 'replicate'), fixtures.padarray_replicate);
  asserts.isequal(padarray(A, [1 1], 'symmetric'), fixtures.padarray_symmetric);
end
if isfield(fixtures.available, 'filtering') && fixtures.available.filtering
  asserts.isapprox(fspecial('gaussian', [3 3], 0.5), fixtures.fspecial_gaussian, 1e-12);
  asserts.isapprox(imfilter(A, ones(3, 3) / 9, 'replicate'), fixtures.imfilter_replicate, 1e-12);
  asserts.isequal(medfilt2([1 100 2; 3 4 5; 6 7 8], [3 3]), fixtures.medfilt2_zero);
  asserts.isequal(medfilt2([1 100 2; 3 4 5; 6 7 8], [3 3], 'symmetric'), fixtures.medfilt2_symmetric);
end
if isfield(fixtures.available, 'imgaussfilt') && fixtures.available.imgaussfilt
  GAUSS_RGB = zeros(2, 2, 3);
  GAUSS_RGB(:, :, 1) = [1 2; 3 4];
  GAUSS_RGB(:, :, 2) = [10 20; 30 40];
  GAUSS_RGB(:, :, 3) = [100 200; 300 400];
  asserts.isapprox(imgaussfilt([1 2; 3 4], 0.5, 'FilterSize', [3 3], 'Padding', 0), ...
    fixtures.imgaussfilt_zero, 1e-12);
  asserts.isapprox(imgaussfilt([1 2; 3 4], [0.5 1.0], 'FilterSize', [3 5], ...
    'Padding', 'symmetric'), fixtures.imgaussfilt_symmetric, 1e-12);
  asserts.isapprox(imgaussfilt(GAUSS_RGB, 0.5, 'FilterSize', [3 3], 'Padding', 0), ...
    fixtures.imgaussfilt_rgb, 1e-10);
  asserts.isequal(imgaussfilt(uint8([1 2; 3 4]), 0.5, 'FilterSize', [1 1]), ...
    fixtures.imgaussfilt_uint8_identity);
  if isfield(fixtures, 'imgaussfilt_int8_identity')
    asserts.isequal(imgaussfilt(int8([1 2; 3 4]) - 3, 0.5, 'FilterSize', [1 1]), ...
      fixtures.imgaussfilt_int8_identity);
  end
end
if isfield(fixtures.available, 'imgaussfilt_rgba') && fixtures.available.imgaussfilt_rgba
  GAUSS_RGBA = zeros(2, 2, 4);
  GAUSS_RGBA(:, :, 1) = [1 2; 3 4];
  GAUSS_RGBA(:, :, 2) = [10 20; 30 40];
  GAUSS_RGBA(:, :, 3) = [100 200; 300 400];
  GAUSS_RGBA(:, :, 4) = [0.1 0.2; 0.3 0.4];
  asserts.isapprox(imgaussfilt(GAUSS_RGBA, 0.5, 'FilterSize', [3 3], 'Padding', 0), ...
    fixtures.imgaussfilt_rgba, 1e-10);
  asserts.isequal(imgaussfilt(uint8(10 * GAUSS_RGBA), 0.5, 'FilterSize', [1 1]), ...
    fixtures.imgaussfilt_rgba_uint8_identity);
end
if isfield(fixtures.available, 'imgaussfilt3') && fixtures.available.imgaussfilt3
  asserts.isequal(imgaussfilt3(V3, 0.5, 'FilterSize', [1 1 1]), ...
    fixtures.imgaussfilt3_identity);
  asserts.isapprox(imgaussfilt3(IMPULSE, 0.5, 'FilterSize', [3 3 3], 'Padding', 0), ...
    fixtures.imgaussfilt3_impulse, 1e-12);
  asserts.isapprox(imgaussfilt3(single(V3), [0.5 0.75 1], 'FilterSize', [3 3 5], ...
    'Padding', 'replicate'), fixtures.imgaussfilt3_single_replicate, 1e-6);
  asserts.isequal(imgaussfilt3(uint8(V3), 0.5, 'FilterSize', [1 1 1]), ...
    fixtures.imgaussfilt3_uint8_identity);
  if isfield(fixtures, 'imgaussfilt3_int8_identity')
    asserts.isequal(imgaussfilt3(int8(V3) - 4, 0.5, 'FilterSize', [1 1 1]), ...
      fixtures.imgaussfilt3_int8_identity);
  end
  if isfield(fixtures, 'imgaussfilt3_complex_identity')
    asserts.isequal(imgaussfilt3(G3_COMPLEX, 0.5, 'FilterSize', [1 1 1]), ...
      fixtures.imgaussfilt3_complex_identity);
  end
end
%=============================================================================
asserts.checkerror('padarray([1 2], [1 1], ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'padding method'));
%=============================================================================
asserts.checkerror('padarray([1 2], -1)', sprintf(_('%s must contain nonnegative integers.'), 'Pad size'));
%=============================================================================
asserts.checkerror('padarray([1 2], [1.5 0])', sprintf(_('%s must contain nonnegative integers.'), 'Pad size'));
%=============================================================================
asserts.checkerror('padarray([1 2], [1 + i 0])', sprintf(_('%s must contain nonnegative integers.'), 'Pad size'));
%=============================================================================
asserts.checkerror('padarray({1 2}, [1 1])', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('padarray(sparse([1 2]), [1 1])', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('padarray(zeros(2, 2, 2), [1 1])', sprintf(_('%s must be a 2-D or RGB array.'), 'Image'));
