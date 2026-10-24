%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
RGB = zeros(1, 3, 3);
RGB(:, :, 1) = [1 0 0];
RGB(:, :, 2) = [0 1 0];
RGB(:, :, 3) = [0 0 1];
G = rgb2gray(RGB);
asserts.isapprox(G, [0.298936021293776 0.587043074451121 0.114020904255103], 1e-12);
HSV = rgb2hsv(RGB);
RGB2 = hsv2rgb(HSV);
asserts.isapprox(RGB2, RGB, 1e-12);
Y = rgb2ycbcr(RGB);
RGB3 = ycbcr2rgb(Y);
asserts.isapprox(RGB3, RGB, 1e-5);
map = [1 0 0; 0 1 0; 0 0 1];
YMAP = rgb2ycbcr(map);
asserts.isequal(size(YMAP), size(map));
asserts.isapprox(ycbcr2rgb(YMAP), map, 1e-5);
asserts.checkerror('rgb2ycbcr(single([1 0 0; 0 1 0]))', ...
  _('Colormap must be a double matrix with values in the range [0, 1].'));
asserts.checkerror('ycbcr2rgb(single([1 0 0; 0 1 0]))', ...
  _('Colormap must be a double matrix with values in the range [0, 1].'));
YUINT8 = rgb2ycbcr(uint8(reshape([255 0 0], [1 1 3])));
asserts.isequal(YUINT8(:)', uint8([81 90 240]));
I = uint8([0 1 2]);
asserts.isapprox(ind2rgb(I, map), RGB, 1e-12);
BW = logical([0 1]);
REF_LOGICAL_RGB = zeros(1, 2, 3);
REF_LOGICAL_RGB(:, :, 1) = [1 0];
REF_LOGICAL_RGB(:, :, 2) = [0 1];
asserts.isapprox(ind2rgb(BW, map(1:2, :)), REF_LOGICAL_RGB, 1e-12);
MAP_EXT = [10 20 30 99; 40 50 60 99; 70 80 90 99; 100 110 120 99];
RGB_DOUBLE_IDX = zeros(1, 4, 3);
RGB_DOUBLE_IDX(:, :, 1) = [10 10 40 100];
RGB_DOUBLE_IDX(:, :, 2) = [20 20 50 110];
RGB_DOUBLE_IDX(:, :, 3) = [30 30 60 120];
asserts.isequal(ind2rgb([0 1 2 5], MAP_EXT), RGB_DOUBLE_IDX);
RGB_UINT_IDX = zeros(1, 4, 3);
RGB_UINT_IDX(:, :, 1) = [10 40 70 100];
RGB_UINT_IDX(:, :, 2) = [20 50 80 110];
RGB_UINT_IDX(:, :, 3) = [30 60 90 120];
asserts.isequal(ind2rgb(uint8([0 1 2 255]), single(MAP_EXT)), RGB_UINT_IDX);
asserts.checkerror('ind2rgb([1.2 2], [1 0 0; 0 1 0])', ...
  _('Indexed image values must be finite integers.'));
RGB_EMPTY = ind2rgb(zeros(0, 2), map);
asserts.isequal(size(RGB_EMPTY), [0 2 3]);
asserts.isequal(class(RGB_EMPTY), 'double');
%=============================================================================
asserts.checkerror('rgb2gray(ones(2, 2))', sprintf(_('%s must be an M-by-N-by-3 array.'), 'RGB image'));
%=============================================================================
RGB = zeros(2, 3, 3);
RGB(:, :, 1) = [1 0 0.2; 0.4 0.5 0.6];
RGB(:, :, 2) = [0 1 0.3; 0.4 0.2 0.6];
RGB(:, :, 3) = [0 0 0.9; 0.4 0.1 0.2];
REF_GRAY = [0.298936021293776 0.587043074451121 0.338518940423684; 0.4 0.278278715962623 0.554391638297959];
asserts.isapprox(rgb2gray(RGB), REF_GRAY, 1e-12);
asserts.isapprox(im2gray(RGB), REF_GRAY, 1e-12);
asserts.isequal(im2gray(uint8([0 255])), uint8([0 255]));
asserts.checkerror('im2gray(logical([0 1]))', sprintf(_('%s must be numeric and non-logical.'), 'Image'));
asserts.checkerror('im2gray(zeros(2, 2, 4))', sprintf(_('%s must be an M-by-N-by-3 array.'), 'RGB image'));
%=============================================================================
MAP = [1 0 0; 0 1 0; 0 0 1; 0.2 0.3 0.9];
REF_GRAY_MAP = [0.298936021293776 0.298936021293776 0.298936021293776; ...
  0.587043074451121 0.587043074451121 0.587043074451121; ...
  0.114020904255103 0.114020904255103 0.114020904255103; ...
  0.338518940423684 0.338518940423684 0.338518940423684];
asserts.isapprox(rgb2gray(MAP), REF_GRAY_MAP, 1e-12);
asserts.isapprox(rgb2gray([2 0 -1; 0 1 0]), ...
  [0.483851138332449 0.483851138332449 0.483851138332449; ...
  0.587043074451121 0.587043074451121 0.587043074451121], 1e-12);
asserts.isequal(rgb2gray(reshape([NaN 0 1], [1 1 3])), 0);
asserts.isequal(rgb2gray(reshape([Inf 0 0], [1 1 3])), 1);
asserts.isequal(rgb2gray([NaN 0 1; Inf 0 0]), [0 0 0; 1 1 1]);
REF_HSV_MAP = [0 1 1; 1 / 3 1 1; 2 / 3 1 1; 9 / 14 7 / 9 0.9];
HSV_MAP = rgb2hsv(MAP);
asserts.isapprox(HSV_MAP, REF_HSV_MAP, 1e-12);
asserts.isapprox(hsv2rgb(HSV_MAP), MAP, 1e-12);
asserts.isequal(class(hsv2rgb(single([0 1 1; 0.5 1 1]))), 'single');
asserts.isapprox(hsv2rgb(single([0 1 1; 0.5 1 1])), single([1 0 0; 0 1 1]), 1e-7);
asserts.isapprox(hsv2rgb(logical([0 1 1; 1 1 1])), [1 0 0; 1 0 0], 1e-15);
asserts.checkerror('rgb2hsv(zeros(0, 3))', sprintf(_('%s must be nonempty.'), 'Colormap'));
asserts.checkerror('hsv2rgb(zeros(0, 3))', sprintf(_('%s must be nonempty.'), 'HSV colormap'));
asserts.checkerror('hsv2rgb([0 2 1])', _('HSV colormap values must be in the range [0, 1].'));
asserts.checkerror('hsv2rgb(uint8([0 1 1]))', sprintf(_('%s must be double, single, or logical.'), 'HSV colormap'));
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
RGB_FIXTURE = zeros(2, 2, 3);
RGB_FIXTURE(:, :, 1) = [1 0; 0 1];
RGB_FIXTURE(:, :, 2) = [0 1; 0 1];
RGB_FIXTURE(:, :, 3) = [0 0; 1 1];
if fixtures.available.rgb2gray
  asserts.isapprox(rgb2gray(RGB_FIXTURE), fixtures.rgb2gray, 1e-12);
  asserts.isapprox(rgb2gray(MAP), fixtures.rgb2gray_map, 1e-12);
  asserts.isequal(rgb2gray(reshape([NaN 0 1], [1 1 3])), fixtures.rgb2gray_nan);
  asserts.isequal(rgb2gray(reshape([Inf 0 0], [1 1 3])), fixtures.rgb2gray_inf);
end
if fixtures.available.im2gray
  asserts.isapprox(im2gray(RGB_FIXTURE), fixtures.im2gray_rgb, 1e-12);
  asserts.isequal(im2gray(uint8([0 255])), fixtures.im2gray_gray_uint8);
  asserts.isequal(im2gray([1 + 2i 0; 0 3 + 4i]), fixtures.im2gray_complex_2d);
  asserts.isequal(issparse(im2gray(sparse([1 0; 0 2]))), issparse(fixtures.im2gray_sparse));
  asserts.isequal(full(im2gray(sparse([1 0; 0 2]))), full(fixtures.im2gray_sparse));
end
if fixtures.available.rgb2hsv
  asserts.isapprox(rgb2hsv(RGB_FIXTURE), fixtures.rgb2hsv, 1e-12);
  asserts.isapprox(rgb2hsv(MAP), fixtures.rgb2hsv_map, 1e-12);
end
if fixtures.available.hsv2rgb
  asserts.isapprox(hsv2rgb(fixtures.rgb2hsv), fixtures.hsv2rgb, 1e-12);
  asserts.isapprox(hsv2rgb(fixtures.rgb2hsv_map), fixtures.hsv2rgb_map, 1e-12);
  asserts.istrue(all(isnan(hsv2rgb(reshape([2 2 1], [1 1 3])))));
  asserts.istrue(all(isnan(fixtures.hsv2rgb_hue_out(:))));
end
if fixtures.available.ind2rgb
  asserts.isapprox(ind2rgb(uint8([0 1 2 255]), MAP), fixtures.ind2rgb_uint8, 1e-12);
  asserts.isapprox(ind2rgb([0 1 2 5], MAP), fixtures.ind2rgb_double, 1e-12);
  asserts.isapprox(ind2rgb([NaN Inf -Inf 0 1 4], MAP), fixtures.ind2rgb_nonfinite, 1e-12);
  asserts.isapprox(ind2gray([NaN Inf -Inf 0 1 4], MAP), rgb2gray(fixtures.ind2rgb_nonfinite), 1e-12);
end
