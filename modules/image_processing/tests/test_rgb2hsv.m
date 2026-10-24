%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.checkerror('rgb2hsv(ones(2, 2))', sprintf(_('%s must be an M-by-N-by-3 array.'), 'RGB image'));
%=============================================================================
RGB = zeros(2, 3, 3);
RGB(:, :, 1) = [1 0 0.2; 0.4 0.5 0.6];
RGB(:, :, 2) = [0 1 0.3; 0.4 0.2 0.6];
RGB(:, :, 3) = [0 0 0.9; 0.4 0.1 0.2];
REF_HSV = zeros(2, 3, 3);
REF_HSV(:, :, 1) = [0 1 / 3 9 / 14; 0 1 / 24 1 / 6];
REF_HSV(:, :, 2) = [1 1 7 / 9; 0 4 / 5 2 / 3];
REF_HSV(:, :, 3) = [1 1 0.9; 0.4 0.5 0.6];
HSV = rgb2hsv(RGB);
asserts.isapprox(HSV, REF_HSV, 1e-12);
asserts.isapprox(hsv2rgb(HSV), RGB, 1e-12);
HSV_EMPTY = rgb2hsv(zeros(0, 2, 3));
asserts.isequal(size(HSV_EMPTY), [0 2 3]);
asserts.isequal(class(HSV_EMPTY), 'double');
HSV_EMPTY_SINGLE = rgb2hsv(single(zeros(2, 0, 3)));
asserts.isequal(size(HSV_EMPTY_SINGLE), [2 0 3]);
asserts.isequal(class(HSV_EMPTY_SINGLE), 'single');
RGB_EMPTY = hsv2rgb(zeros(0, 2, 3));
asserts.isequal(size(RGB_EMPTY), [0 2 3]);
asserts.isequal(class(RGB_EMPTY), 'double');
RGB_EMPTY_SINGLE = hsv2rgb(single(zeros(2, 0, 3)));
asserts.isequal(size(RGB_EMPTY_SINGLE), [2 0 3]);
asserts.isequal(class(RGB_EMPTY_SINGLE), 'single');
%=============================================================================
TIE_RGB = zeros(1, 3, 3);
TIE_RGB(:, :, 1) = [1 1 0];
TIE_RGB(:, :, 2) = [1 0 1];
TIE_RGB(:, :, 3) = [0 1 1];
TIE_HSV = rgb2hsv(TIE_RGB);
asserts.isapprox(TIE_HSV(:, :, 1), [1 / 6 5 / 6 1 / 2], 1e-12);
asserts.isapprox(TIE_HSV(:, :, 2), [1 1 1], 1e-12);
asserts.isapprox(TIE_HSV(:, :, 3), [1 1 1], 1e-12);
%=============================================================================
SINGLE_RGB = single(zeros(1, 1, 3));
SINGLE_RGB(:, :, 1) = 1;
SINGLE_HSV = rgb2hsv(SINGLE_RGB);
asserts.isequal(class(SINGLE_HSV), 'single');
asserts.isequal(class(hsv2rgb(SINGLE_HSV)), 'single');
UINT8_RGB = uint8(zeros(1, 2, 3));
UINT8_RGB(:, :, 1) = [255 0];
UINT8_RGB(:, :, 2) = [0 255];
asserts.isequal(rgb2gray(UINT8_RGB), uint8([76 150]));
asserts.isequal(rgb2gray(int8(reshape([127 -128 -128], [1 1 3]))), int8(-52));
asserts.isequal(rgb2gray(int16(reshape([32767 -32768 -32768], [1 1 3]))), int16(-13177));
asserts.isequal(rgb2gray(uint32(reshape([intmax('uint32') 0 0], [1 1 3]))), uint32(1283920435));
asserts.isequal(rgb2gray(int32(reshape([intmax('int32') intmin('int32') intmin('int32')], [1 1 3]))), int32(-863563213));
asserts.checkerror('rgb2gray(logical(reshape([1 0 0], [1 1 3])))', sprintf(_('%s must be numeric and non-logical.'), 'RGB image'));
UINT8_HSV = rgb2hsv(UINT8_RGB);
asserts.isequal(class(UINT8_HSV), 'double');
asserts.isapprox(UINT8_HSV(:, :, 1), [0 1 / 3], 1e-15);
asserts.isapprox(UINT8_HSV(:, :, 2), [1 1], 1e-15);
asserts.isapprox(UINT8_HSV(:, :, 3), [1 1], 1e-15);
OUT_RGB = zeros(1, 1, 3);
OUT_RGB(:, :, 1) = 2;
OUT_HSV = rgb2hsv(OUT_RGB);
asserts.isapprox(reshape(OUT_HSV, [1 3]), [0 1 2], 1e-15);
asserts.checkerror('rgb2hsv(logical(reshape([1 0 0], [1 1 3])))', _('RGB image must be double, single, uint8, or uint16.'));
asserts.checkerror('rgb2hsv(int16(reshape([32767 -32768 -32768], [1 1 3])))', _('RGB image must be double, single, uint8, or uint16.'));
asserts.checkerror('rgb2hsv(uint32(reshape([intmax(''uint32'') 0 0], [1 1 3])))', _('RGB image must be double, single, uint8, or uint16.'));
asserts.checkerror('rgb2hsv(reshape([1+2i 0 0], [1 1 3]))', sprintf(_('%s must be real.'), 'RGB image'));
LOGICAL_HSV = false(1, 1, 3);
LOGICAL_HSV(:, :, 2) = true;
LOGICAL_HSV(:, :, 3) = true;
asserts.isequal(class(hsv2rgb(LOGICAL_HSV)), 'double');
asserts.isapprox(hsv2rgb(LOGICAL_HSV), reshape([1 0 0], [1 1 3]), 1e-15);
OUT_HSV = zeros(1, 1, 3);
OUT_HSV(:, :, 2) = 2;
OUT_HSV(:, :, 3) = 1;
asserts.isapprox(reshape(hsv2rgb(OUT_HSV), [1 3]), [1 -1 -1], 1e-15);
OUT_HSV(:, :, 1) = 2;
asserts.istrue(all(isnan(reshape(hsv2rgb(OUT_HSV), [1 3]))));
asserts.checkerror('hsv2rgb(reshape([1+2i 0 0], [1 1 3]))', sprintf(_('%s must be real.'), 'HSV image'));
asserts.checkerror('hsv2rgb(uint8(zeros(1, 1, 3)))', sprintf(_('%s must be double, single, or logical.'), 'HSV image'));
asserts.checkerror('rgb2gray(uint8([255 0 0; 0 255 0]))', ...
  _('Colormap must be a double matrix with values in the range [0, 1].'));
asserts.checkerror('rgb2hsv(single([1 0 0; 0 1 0]))', ...
  _('Colormap must be a double matrix with values in the range [0, 1].'));
