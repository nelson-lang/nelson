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
I = reshape(1:9, 3, 3);
T = imtranslate(I, [1 0], 'Interpolation', 'nearest');
asserts.isequal(size(T), size(I));
asserts.isequal(T, [0 1 4; 0 2 5; 0 3 6]);
T_CASE = imtranslate(I, [1 0], 'INTERPOLATION', 'Nearest');
asserts.isequal(T_CASE, T);
T2 = imtranslate(I, [1 0], 'nearest');
asserts.isequal(T2, T);
T_CUBIC = imtranslate(I, [0 0], 'cubic');
asserts.isequal(T_CUBIC, I);
asserts.isequal(imtranslate(I, [-1 0], 'nearest', 'OutputView', 'full'), [1 4 7 0; 2 5 8 0; 3 6 9 0]);
asserts.isequal(imtranslate(I, [1 0], 'nearest', 'OutputView', 'full'), [0 1 4 7; 0 2 5 8; 0 3 6 9]);
asserts.isequal(imtranslate(I, [0 1], 'nearest', 'OutputView', 'full'), [0 0 0; 1 4 7; 2 5 8; 3 6 9]);
RGB_FILL = zeros(2, 2, 3);
RGB_FILL(:, :, 1) = 10;
RGB_FILL(:, :, 2) = 20;
RGB_FILL(:, :, 3) = 30;
RGB_SHIFT = imtranslate(RGB_FILL, [1 0], 'nearest', 'FillValues', [1 2 3]);
asserts.isequal(reshape(RGB_SHIFT(1, 1, :), [1 3]), [1 2 3]);
W = imwarp(I, eye(3), 'Interpolation', 'nearest');
asserts.isequal(W, I);
W2 = imwarp(I, eye(3), 'nearest');
asserts.isequal(W2, I);
W_CASE = imwarp(I, eye(3), 'INTERPOLATION', 'Nearest');
asserts.isequal(W_CASE, I);
W_STRING_LINEAR = imwarp(single(I), eye(3), string('Interpolation'), string('Linear'));
asserts.isequal(class(W_STRING_LINEAR), 'single');
asserts.isequal(W_STRING_LINEAR, single(I));
W_CUBIC = imwarp(I, eye(3), 'cubic');
asserts.isequal(W_CUBIC, I);
W_RGB = imwarp(RGB_FILL, [1 0 0; 0 1 0; 1 0 1], 'nearest', 'FillValues', [4 5 6], ...
  'OutputView', 'same');
asserts.isequal(reshape(W_RGB(1, 1, :), [1 3]), [4 5 6]);
W_STRING_OPTIONS = imwarp(I, [1 0 0; 0 1 0; 1 0 1], string('Nearest'), ...
  string('FillValues'), int8(9), string('OutputView'), string('same'));
asserts.isequal(W_STRING_OPTIONS, [9 1 4; 9 2 5; 9 3 6]);
RGBA_FILL = cat(3, RGB_FILL, 40 * ones(2, 2));
W_RGBA = imwarp(RGBA_FILL, [1 0 0; 0 1 0; 1 0 1], 'nearest', ...
  'FillValues', [4 5 6 7], 'OutputView', 'same');
asserts.isequal(size(W_RGBA), [2 2 4]);
asserts.isequal(reshape(W_RGBA(1, 1, :), [1 4]), [4 5 6 7]);
W_RGBA_UINT8 = imwarp(uint8(RGBA_FILL), eye(3), 'nearest');
asserts.isequal(class(W_RGBA_UINT8), 'uint8');
asserts.isequal(W_RGBA_UINT8(:, :, 4), uint8(40 * ones(2, 2)));
W_UINT8 = imwarp(uint8(I), eye(3), 'nearest');
asserts.isequal(class(W_UINT8), 'uint8');
asserts.isequal(W_UINT8, uint8(I));
W_UINT16 = imwarp(uint16(I), eye(3), 'nearest');
asserts.isequal(class(W_UINT16), 'uint16');
asserts.isequal(W_UINT16, uint16(I));
W_INT16 = imwarp(int16(I), eye(3), 'nearest');
asserts.isequal(class(W_INT16), 'int16');
asserts.isequal(W_INT16, int16(I));
W_INT8 = imwarp(int8(I) - 5, eye(3), 'nearest');
asserts.isequal(class(W_INT8), 'int8');
asserts.isequal(W_INT8, int8(I) - 5);
W_UINT32 = imwarp(uint32(I), eye(3), 'nearest');
asserts.isequal(class(W_UINT32), 'uint32');
asserts.isequal(W_UINT32, uint32(I));
W_UINT64 = imwarp(uint64(I), eye(3), 'nearest');
asserts.isequal(class(W_UINT64), 'uint64');
asserts.isequal(W_UINT64, uint64(I));
W_INT32 = imwarp(int32(I), eye(3), 'nearest');
asserts.isequal(class(W_INT32), 'int32');
asserts.isequal(W_INT32, int32(I));
W_INT64 = imwarp(int64(I), eye(3), 'nearest');
asserts.isequal(class(W_INT64), 'int64');
asserts.isequal(W_INT64, int64(I));
W_SINGLE = imwarp(single(I), eye(3), 'linear');
asserts.isequal(class(W_SINGLE), 'single');
asserts.isequal(W_SINGLE, single(I));
W_BILINEAR = imwarp(single(I), eye(3), 'bilinear');
asserts.isequal(W_BILINEAR, W_SINGLE);
W_LOGICAL = imwarp(logical(I > 4), eye(3), 'nearest');
asserts.isequal(class(W_LOGICAL), 'logical');
asserts.isequal(W_LOGICAL, logical(I > 4));
W_LOGICAL_FILL = imwarp(logical([1 0; 0 1]), [1 0 0; 0 1 0; 1 0 1], 'nearest', ...
  'FillValues', true, 'OutputView', 'same');
asserts.isequal(class(W_LOGICAL_FILL), 'logical');
asserts.isequal(W_LOGICAL_FILL, logical([1 1; 1 0]));
W_NAN = imwarp([1 2 3; 4 NaN 6; 7 8 9], eye(3), 'nearest');
asserts.istrue(isnan(W_NAN(2, 2)));
W_INF = imwarp([1 2 3; 4 Inf 6; 7 8 9], eye(3), 'nearest');
asserts.istrue(isinf(W_INF(2, 2)));
W_COMPLEX = imwarp([1 + i 2; 3 4 - 2i], eye(3), 'nearest');
asserts.isequal(W_COMPLEX, [1 + i 2; 3 4 - 2i]);
W_LINEAR_SHIFT = imwarp(I, [1 0 0; 0 1 0; 0.5 0 1], 'linear', ...
  'FillValues', -1, 'OutputView', 'same');
asserts.isapprox(W_LINEAR_SHIFT, [-1 2.5 5.5; -1 3.5 6.5; -1 4.5 7.5], 1e-14);
%=============================================================================
