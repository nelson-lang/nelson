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
% <--ADV-CLI MODE-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
R = reshape(1:18, [2 3 3]);
REF_90(:, :, 1) = [5 6; 3 4; 1 2];
REF_90(:, :, 2) = [11 12; 9 10; 7 8];
REF_90(:, :, 3) = [17 18; 15 16; 13 14];
REF_180(:, :, 1) = [6 4 2; 5 3 1];
REF_180(:, :, 2) = [12 10 8; 11 9 7];
REF_180(:, :, 3) = [18 16 14; 17 15 13];
REF_270(:, :, 1) = [2 1; 4 3; 6 5];
REF_270(:, :, 2) = [8 7; 10 9; 12 11];
REF_270(:, :, 3) = [14 13; 16 15; 18 17];
%=============================================================================
asserts.isequal(imrotate(R, 0), R);
asserts.isequal(imrotate(R, 360), R);
asserts.isequal(imrotate(R, 90), REF_90);
asserts.isequal(imrotate(R, -90), REF_270);
asserts.isequal(imrotate(R, 270), REF_270);
asserts.isequal(imrotate(R, -270), REF_90);
asserts.isequal(imrotate(R, 180), imrotate(R, -180))
asserts.isequal(imrotate(R, 180), REF_180);
asserts.isequal(imrotate(R, -180), REF_180);
%=============================================================================
REF = [0 1 1 0; 0 1 1 0];
R = imrotate(ones(2, 4), 90, 'nearest', 'crop');
asserts.isequal(R, REF);
%=============================================================================
REF = [1 1; 1 1; 1 1; 1 1];
R = imrotate(ones(2, 4), 90, 'nearest', 'loose');
asserts.isequal(R, REF);
%=============================================================================
A = reshape(1:16, [4 4]);
R = imrotate(A, 33, 'nearest', 'crop');
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), size(A));
asserts.istrue(all(R(:) == 0 | ismember(R(:), A(:))));
AU8 = uint8(A);
RU8 = imrotate(AU8, -33, 'nearest', 'loose');
asserts.isequal(class(RU8), 'uint8');
asserts.istrue(all(double(RU8(:)) == 0 | ismember(double(RU8(:)), double(AU8(:)))));
RGB64 = uint8(reshape(mod(1:(64 * 64 * 3), 256), [64 64 3]));
RRGB = imrotate(RGB64, 33, 'bilinear', 'crop');
asserts.isequal(class(RRGB), 'uint8');
asserts.isequal(size(RRGB), size(RGB64));
asserts.istrue(any(RRGB(:) ~= 0));
RRGB_CASE = imrotate(RGB64, 33, 'Bilinear', 'Crop');
asserts.isequal(RRGB_CASE, RRGB);
RGBA64 = uint8(reshape(mod(1:(64 * 64 * 4), 256), [64 64 4]));
RRGBA = imrotate(RGBA64, 33, 'bilinear', 'crop');
asserts.isequal(class(RRGBA), 'uint8');
asserts.isequal(size(RRGBA), size(RGBA64));
asserts.istrue(any(RRGBA(:, :, 4) ~= 0, 'all'));
RGBA90 = imrotate(RGBA64, 90, 'nearest', 'loose');
asserts.isequal(size(RGBA90), [64 64 4]);
asserts.isequal(RGBA90(:, :, 4), imrotate(RGBA64(:, :, 4), 90, 'nearest', 'loose'));
asserts.isequal(imrotate(logical([1 0; 0 1]), 180), logical([1 0; 0 1]));
S = single([1 NaN; Inf 4]);
S180 = imrotate(S, 180, 'nearest', 'loose');
asserts.isequal(class(S180), 'single');
asserts.istrue(isnan(S180(2, 1)));
asserts.istrue(isinf(S180(1, 2)));
U16 = uint16(reshape(1:12, [3 4]));
U16_90 = imrotate(U16, 90, 'nearest', 'loose');
asserts.isequal(class(U16_90), 'uint16');
asserts.isequal(U16_90, uint16([10 11 12; 7 8 9; 4 5 6; 1 2 3]));
I8 = int8(reshape(-6:5, [3 4]));
I8_90 = imrotate(I8, 90, 'nearest', 'loose');
asserts.isequal(class(I8_90), 'int8');
asserts.isequal(I8_90, int8([3 4 5; 0 1 2; -3 -2 -1; -6 -5 -4]));
U32 = uint32(reshape(1:12, [3 4]));
U32_90 = imrotate(U32, 90, 'nearest', 'loose');
asserts.isequal(class(U32_90), 'uint32');
asserts.isequal(U32_90, uint32([10 11 12; 7 8 9; 4 5 6; 1 2 3]));
U64 = uint64(reshape(1:12, [3 4]));
U64_180 = imrotate(U64, 180, 'nearest', 'loose');
asserts.isequal(class(U64_180), 'uint64');
asserts.isequal(U64_180, uint64([12 9 6 3; 11 8 5 2; 10 7 4 1]));
I32 = int32(reshape(1:12, [3 4]) - 6);
I32_270 = imrotate(I32, 270, 'nearest', 'loose');
asserts.isequal(class(I32_270), 'int32');
asserts.isequal(I32_270, int32([-3 -4 -5; 0 -1 -2; 3 2 1; 6 5 4]));
I64 = int64(reshape(1:12, [3 4]) - 6);
I64_180 = imrotate(I64, 180, 'nearest', 'loose');
asserts.isequal(class(I64_180), 'int64');
asserts.isequal(I64_180, int64([6 3 0 -3; 5 2 -1 -4; 4 1 -2 -5]));
RGBA_DOUBLE = zeros(2, 3, 4);
RGBA_DOUBLE(:, :, 1) = reshape(1:6, [2 3]);
RGBA_DOUBLE(:, :, 2) = 10 + reshape(1:6, [2 3]);
RGBA_DOUBLE(:, :, 3) = 20 + reshape(1:6, [2 3]);
RGBA_DOUBLE(:, :, 4) = [0.1 0.3 0.5; 0.2 0.4 0.6];
RGBA_180 = imrotate(RGBA_DOUBLE, 180, 'nearest', 'loose');
asserts.isequal(RGBA_180(:, :, 4), [0.6 0.4 0.2; 0.5 0.3 0.1]);
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'imrotate') && fixtures.available.imrotate
  ROT_INPUT = reshape(1:12, 3, 4);
  asserts.isequal(imrotate(uint16(ROT_INPUT), 90, 'nearest', 'loose'), ...
    fixtures.imrotate_uint16_90_loose);
  if isfield(fixtures, 'imrotate_int8_90_loose')
    asserts.isequal(imrotate(int8(ROT_INPUT) - 7, 90, 'nearest', 'loose'), ...
      fixtures.imrotate_int8_90_loose);
  end
  ROT_RGB = zeros(4, 4, 3);
  ROT_RGB(:, :, 1) = reshape(1:16, 4, 4);
  ROT_RGB(:, :, 2) = 10 + reshape(1:16, 4, 4);
  ROT_RGB(:, :, 3) = 20 + reshape(1:16, 4, 4);
  asserts.isapprox(imrotate(ROT_RGB, 33, 'bilinear', 'crop'), ...
    fixtures.imrotate_rgb_bilinear_crop, 1e-12);
  ROT_RGBA = zeros(4, 4, 4);
  ROT_RGBA(:, :, 1:3) = ROT_RGB;
  ROT_RGBA(:, :, 4) = reshape((1:16) / 16, 4, 4);
  if isfield(fixtures.available, 'imrotate_rgba') && fixtures.available.imrotate_rgba
    asserts.isequal(imrotate(ROT_RGBA, -45, 'nearest', 'crop'), ...
      fixtures.imrotate_rgba_nearest_crop);
  end
  ROT_NONFINITE = imrotate(single([1 NaN; Inf 4]), 180, 'nearest', 'loose');
  asserts.isequal(class(ROT_NONFINITE), class(fixtures.imrotate_single_nonfinite));
  asserts.istrue(isnan(ROT_NONFINITE(2, 1)));
  asserts.istrue(isinf(ROT_NONFINITE(1, 2)));
end
%=============================================================================
if exist('figure') ~= 0 && exist('refresh') ~= 0 && exist('imshow') ~= 0 && exist('image') ~= 0
  RGB_GUI = uint8(reshape(mod(1:(16 * 16 * 3), 256), [16 16 3]));
  RGB_GUI_ROT = imrotate(RGB_GUI, 17, 'bilinear', 'crop');
  fig_gui = figure('Visible', 'off');
  img_gui = imshow(RGB_GUI);
  drawnow();
  set(img_gui, 'CData', RGB_GUI_ROT);
  refresh(fig_gui);
  asserts.isequal(size(img_gui.CData), [16 16 3]);
  asserts.isequal(double(img_gui.CData), double(RGB_GUI_ROT));
  delete(fig_gui);
  drawnow();
  RGBA_GUI = uint8(reshape(mod(1:(16 * 16 * 4), 256), [16 16 4]));
  RGBA_GUI_ROT = imrotate(RGBA_GUI, -17, 'bilinear', 'crop');
  fig_rgba_gui = figure('Visible', 'off');
  img_rgba_gui = image('CData', RGBA_GUI);
  drawnow();
  set(img_rgba_gui, 'CData', RGBA_GUI_ROT);
  refresh(fig_rgba_gui);
  asserts.isequal(size(img_rgba_gui.CData), [16 16 4]);
  asserts.isequal(double(img_rgba_gui.CData), double(RGBA_GUI_ROT));
  delete(fig_rgba_gui);
  drawnow();
end
%=============================================================================
