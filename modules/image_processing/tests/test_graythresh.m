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
A = [0 0 1 1];
[level, em] = graythresh(A);
asserts.istrue(level >= 0);
asserts.istrue(level <= 1);
asserts.istrue(em >= 0);
asserts.istrue(em <= 1);
BW = imbinarize(A, 0.5);
asserts.isequal(BW, logical([0 0 1 1]));
BW_GLOBAL = imbinarize(A, 'global');
asserts.isequal(BW_GLOBAL, imbinarize(A));
T = [0.1 0.1 0.9 0.9];
asserts.isequal(imbinarize(A, T), logical([0 0 1 1]));
I_ADAPT = [0.1 0.1 0.1; 0.1 0.9 0.1; 0.1 0.1 0.1];
BW_ADAPT = imbinarize(I_ADAPT, 'adaptive', 'Sensitivity', 0.4, 'ForegroundPolarity', 'bright');
asserts.istrue(BW_ADAPT(2, 2));
BW_DARK = imbinarize(1 - I_ADAPT, 'adaptive', 'ForegroundPolarity', 'dark');
asserts.istrue(BW_DARK(2, 2));
T_ADAPT = adaptthresh(I_ADAPT, 0.4, 'ForegroundPolarity', 'bright');
asserts.isequal(size(T_ADAPT), size(I_ADAPT));
asserts.isapprox(T_ADAPT(2, 2), mean(I_ADAPT(:)) * 0.6, 1e-15);
asserts.isequal(imbinarize(I_ADAPT, 'adaptive', 'Sensitivity', 0.4), I_ADAPT > T_ADAPT);
I_AC = zeros(7, 7);
I_AC(3:5, 3:5) = 1;
MASK_AC = false(7, 7);
MASK_AC(4, 4) = true;
BW_AC = activecontour(I_AC, MASK_AC, 8);
asserts.isequal(BW_AC, logical(I_AC));
asserts.isequal(activecontour(I_AC, MASK_AC, 0), MASK_AC);
asserts.isequal(activecontour(I_AC, MASK_AC, 8, 'SmoothFactor', 0), BW_AC);
BW_AC_EDGE = activecontour(I_AC, MASK_AC, 8, 'edge');
asserts.isequal(BW_AC_EDGE(4, 4), true);
asserts.isequal(class(BW_AC_EDGE), 'logical');
asserts.isequal(activecontour(uint8(I_AC * 255), MASK_AC, 8), logical(I_AC));
asserts.isequal(activecontour(int8(I_AC * 10), MASK_AC, 8), logical(I_AC));
asserts.isequal(activecontour(I_AC, MASK_AC, 'edge'), activecontour(I_AC, MASK_AC, 100, 'edge'));
BW_AC_OPTIONS = activecontour(I_AC, MASK_AC, 8, 'Chan-Vese', 'SmoothFactor', 0, 'ContractionBias', 0);
asserts.isequal(BW_AC_OPTIONS, BW_AC);
BW_AC_EDGE_OPTIONS = activecontour(I_AC, MASK_AC, 'Edge', 'SmoothFactor', 0);
asserts.isequal(class(BW_AC_EDGE_OPTIONS), 'logical');
asserts.isequal(size(BW_AC_EDGE_OPTIONS), size(I_AC));
BW_AC_NAME_VALUE = activecontour(I_AC, MASK_AC, 'Iterations', 8, 'Method', 'edge', 'SmoothFactor', 0);
asserts.isequal(BW_AC_NAME_VALUE, BW_AC_EDGE_OPTIONS);
BW_AC_CONTRACT = activecontour(I_AC, MASK_AC, 8, 'ContractionBias', 1);
asserts.istrue(sum(BW_AC_CONTRACT(:)) <= sum(BW_AC(:)));
MASK_AC_NUMERIC = zeros(7, 7);
MASK_AC_NUMERIC(4, 4) = 1;
MASK_AC_SINGLE = single(MASK_AC_NUMERIC);
asserts.isequal(activecontour(I_AC, MASK_AC_SINGLE, 0), MASK_AC);
asserts.isequal(activecontour(I_AC, MASK_AC_SINGLE, 'Iterations', 8), BW_AC);
BW_AC_STRING_OPTIONS = activecontour(single(I_AC), MASK_AC_NUMERIC, string('Iterations'), uint8(8), ...
  string('Method'), string('chan-vese'), string('SmoothFactor'), 0, string('ContractionBias'), int8(0));
asserts.isequal(BW_AC_STRING_OPTIONS, BW_AC);
BW_AC_LOGICAL_IMAGE = activecontour(logical(I_AC), MASK_AC_NUMERIC, 8, string('Method'), string('edge'));
asserts.isequal(class(BW_AC_LOGICAL_IMAGE), 'logical');
asserts.isequal(size(BW_AC_LOGICAL_IMAGE), size(I_AC));
I_AC_RGB = cat(3, I_AC, I_AC, I_AC);
BW_AC_RGB = activecontour(I_AC_RGB, MASK_AC, 8);
asserts.isequal(BW_AC_RGB, BW_AC);
asserts.isequal(activecontour(I_AC_RGB, MASK_AC, 8, 'Method', 'Chan-Vese'), BW_AC);
asserts.checkerror('activecontour(I_AC_RGB, MASK_AC, 8, ''edge'')', ...
  _('Edge method is not supported for RGB or RGBA images.'));
I_AC_RGBA = cat(3, I_AC_RGB, 0.5 * ones(size(I_AC)));
asserts.isequal(activecontour(I_AC_RGBA, MASK_AC, 8), BW_AC);
asserts.checkerror('activecontour(I_AC_RGBA, MASK_AC, 8, ''edge'')', ...
  _('Edge method is not supported for RGB or RGBA images.'));
I_AC_RGBA_ALPHA_NAN = I_AC_RGBA;
I_AC_RGBA_ALPHA_NAN(:, :, 4) = NaN;
asserts.isequal(activecontour(I_AC_RGBA_ALPHA_NAN, MASK_AC, 8), BW_AC);
T_DARK = adaptthresh(1 - I_ADAPT, 0.2, 'ForegroundPolarity', 'dark', 'NeighborhoodSize', [3 3]);
asserts.isapprox(T_DARK(2, 2), mean(1 - I_ADAPT(:)) * 1.2, 1e-15);
T_MEDIAN = adaptthresh(I_ADAPT, 0.2, 'Statistic', 'median', 'NeighborhoodSize', [3 3]);
asserts.isapprox(T_MEDIAN(2, 2), 0.08, 1e-15);
T_GAUSS = adaptthresh(I_ADAPT, 0.4, 'Statistic', 'gaussian', 'NeighborhoodSize', [3 3]);
asserts.isequal(size(T_GAUSS), size(I_ADAPT));
asserts.istrue(all(T_GAUSS(:) >= 0) && all(T_GAUSS(:) <= 1));
I_WS = [1 2 3; 2 3 2; 3 2 1];
L_WS = watershed(I_WS, 4);
asserts.isequal(L_WS, [1 1 0; 1 0 2; 0 2 2]);
asserts.isequal(watershed(ones(2, 3), 8), ones(2, 3));
asserts.isequal(class(watershed(uint8(I_WS), 4)), 'double');
asserts.isequal(watershed(single(I_WS), 4), L_WS);
asserts.isequal(watershed(int16(I_WS), 4), L_WS);
asserts.isequal(watershed(logical([1 0 1; 0 0 0; 1 0 1])), ones(3, 3));
asserts.isequal(watershed([1 2 1], 4), [1 0 2]);
I_WS_PLATEAU = [1 2 1; 2 2 2; 1 2 1];
asserts.isequal(watershed(I_WS_PLATEAU, 4), [1 0 3; 0 0 0; 2 0 4]);
asserts.isequal(watershed([1 1 2; 1 2 2], 4), ones(2, 3));
asserts.isequal(watershed([1 2; 2 1], uint8(4)), [1 0; 0 2]);
C4 = logical([0 1 0; 1 1 1; 0 1 0]);
C8 = true(3, 3);
asserts.isequal(watershed(I_WS, C4), watershed(I_WS, 4));
asserts.isequal(watershed(I_WS, C8), watershed(I_WS, 8));
asserts.isequal(watershed(I_WS, single(C4)), watershed(I_WS, 4));
asserts.isequal(watershed(I_WS, single(C8)), watershed(I_WS, 8));
I_WS_SIGNED = int8([1 2 1; 2 3 2; 1 2 1]);
L_WS_SIGNED = watershed(I_WS_SIGNED, C4);
asserts.isequal(class(L_WS_SIGNED), 'double');
asserts.isequal(L_WS_SIGNED, [1 0 3; 0 5 0; 2 0 4]);
asserts.isequal(watershed(int8(I_WS), 4), L_WS);
V_WS = ones(2, 2, 2);
asserts.isequal(watershed(V_WS), V_WS);
asserts.isequal(watershed(uint8(V_WS), 26), V_WS);
C6 = false(3, 3, 3);
C6(2, 2, 2) = true;
C6(1, 2, 2) = true; C6(3, 2, 2) = true;
C6(2, 1, 2) = true; C6(2, 3, 2) = true;
C6(2, 2, 1) = true; C6(2, 2, 3) = true;
C18 = true(3, 3, 3);
C18(1, 1, 1) = false; C18(1, 1, 3) = false;
C18(1, 3, 1) = false; C18(1, 3, 3) = false;
C18(3, 1, 1) = false; C18(3, 1, 3) = false;
C18(3, 3, 1) = false; C18(3, 3, 3) = false;
asserts.isequal(watershed(V_WS, C6), watershed(V_WS, 6));
asserts.isequal(watershed(V_WS, C18), watershed(V_WS, 18));
asserts.isequal(watershed(V_WS, true(3, 3, 3)), watershed(V_WS, 26));
asserts.isequal(watershed(V_WS, single(C6)), watershed(V_WS, 6));
asserts.isequal(watershed(V_WS, single(C18)), watershed(V_WS, 18));
asserts.isequal(watershed(V_WS, single(true(3, 3, 3))), watershed(V_WS, 26));
RM = imregionalmin(I_WS, C4);
asserts.isequal(RM, logical([1 0 0; 0 0 0; 0 0 1]));
RM_PLATEAU = imregionalmin([2 2 2; 2 1 1; 2 1 1]);
asserts.isequal(RM_PLATEAU, logical([0 0 0; 0 1 1; 0 1 1]));
RM_CONN = imregionalmin([2 2 3; 2 2 3; 3 3 1], 4);
asserts.isequal(RM_CONN, logical([1 1 0; 1 1 0; 0 0 1]));
RM_CONN8 = imregionalmin([2 2 3; 2 2 3; 3 3 1], 8);
asserts.isequal(RM_CONN8, logical([0 0 0; 0 0 0; 0 0 1]));
RM_DIAG4 = imregionalmin([1 2; 2 0], 4);
asserts.isequal(RM_DIAG4, logical([1 0; 0 1]));
RM_DIAG8 = imregionalmin([1 2; 2 0], 8);
asserts.isequal(RM_DIAG8, logical([0 0; 0 1]));
HMIN_ZERO = imhmin(I_WS, 0, C4);
asserts.isequal(HMIN_ZERO, I_WS);
HMIN_U8 = imhmin(uint8([5 5 5; 5 2 5; 5 5 5]), 2);
asserts.isequal(class(HMIN_U8), 'uint8');
asserts.isequal(HMIN_U8, uint8([5 5 5; 5 4 5; 5 5 5]));
EXT_ZERO = imextendedmin(I_WS, 0, C4);
asserts.isequal(EXT_ZERO, RM);
EXT_FLAT = imextendedmin([3 3 3; 3 2 3; 3 3 3], 2);
asserts.isequal(EXT_FLAT, true(3, 3));
RMAX = imregionalmax(-I_WS, C4);
asserts.isequal(RMAX, RM);
RMAX_CONN = imregionalmax(-[2 2 3; 2 2 3; 3 3 1], 4);
asserts.isequal(RMAX_CONN, RM_CONN);
RMAX_CONN8 = imregionalmax(-[2 2 3; 2 2 3; 3 3 1], 8);
asserts.isequal(RMAX_CONN8, RM_CONN8);
HMAX_ZERO = imhmax(-I_WS, 0, C4);
asserts.isequal(HMAX_ZERO, -I_WS);
HMAX_U8 = imhmax(uint8([1 1 1; 1 5 1; 1 1 1]), 2);
asserts.isequal(class(HMAX_U8), 'uint8');
asserts.isequal(HMAX_U8, uint8([1 1 1; 1 3 1; 1 1 1]));
EMAX_ZERO = imextendedmax(-I_WS, 0, C4);
asserts.isequal(EMAX_ZERO, RM);
EMAX_FLAT = imextendedmax([1 1 1; 1 3 1; 1 1 1], 2);
asserts.isequal(EMAX_FLAT, true(3, 3));
MASK_REC = false(5, 5);
MASK_REC(2:4, 2:4) = true;
MARKER_REC = false(5, 5);
MARKER_REC(3, 3) = true;
asserts.isequal(imreconstruct(MARKER_REC, MASK_REC, C4), MASK_REC);
MASK_GRAY = [0 1 1 0; 1 2 2 1; 1 2 3 1; 0 1 1 0];
MARKER_GRAY = zeros(4, 4);
MARKER_GRAY(3, 3) = 3;
asserts.isequal(imreconstruct(MARKER_GRAY, MASK_GRAY, C8), MASK_GRAY);
REC_U8 = imreconstruct(uint8([0 0; 0 2]), uint8([0 2; 2 2]));
asserts.isequal(class(REC_U8), 'uint8');
asserts.isequal(REC_U8, uint8([0 2; 2 2]));
I_GRAYCONN = [0 0.1 0.9; 0.1 0.15 0.9; 0.9 0.9 0.2];
GC4 = grayconnected(I_GRAYCONN, 2, 2, 0.1, 4);
GC8 = grayconnected(I_GRAYCONN, 2, 2, 0.1, 8);
asserts.isequal(GC4, logical([0 1 0; 1 1 0; 0 0 0]));
asserts.isequal(GC8, logical([0 1 0; 1 1 0; 0 0 1]));
GC_UINT8 = grayconnected(uint8([10 12 200; 12 14 200; 200 200 15]), 2, 2, 5 / 255, 8);
asserts.isequal(GC_UINT8, logical([1 1 0; 1 1 0; 0 0 1]));
MARKERS = false(3, 3);
MARKERS(1, 1) = true;
IMPOSED = imimposemin(I_WS, MARKERS, C4);
TEMP = IMPOSED;
TEMP(MARKERS) = Inf;
asserts.istrue(IMPOSED(1, 1) < min(TEMP(:)));
IMPOSED_U8 = imimposemin(uint8([5 4; 3 2]), logical([1 0; 0 0]));
asserts.isequal(class(IMPOSED_U8), 'uint8');
asserts.isequal(IMPOSED_U8(1, 1), uint8(0));
asserts.isequal(imimposemin(I_WS, single(MARKERS), C4), IMPOSED);
asserts.isequal(imimposemin(I_WS, false(size(I_WS))), I_WS);
RM_IMPOSED = imregionalmin(IMPOSED, C4);
asserts.istrue(RM_IMPOSED(1, 1));
[counts, bins] = imhist(uint8([0 255]), 2);
asserts.isequal(counts, [1; 1]);
asserts.isequal(bins, [0; 255]);
asserts.isequal(imhist(uint8([0 255]), 1), 2);
asserts.isequal(imhist(logical([0 1 1])), [1; 2]);
limits = stretchlim([0 0.5 1], [0 1]);
asserts.isequal(limits, [0; 1]);
RGB_LIMITS = zeros(2, 2, 3);
RGB_LIMITS(:, :, 1) = [0 0.25; 0.5 1];
RGB_LIMITS(:, :, 2) = [0.1 0.2; 0.8 0.9];
RGB_LIMITS(:, :, 3) = [0.3 0.4; 0.6 0.7];
asserts.isequal(stretchlim(RGB_LIMITS, [0 1]), [0 0.1 0.3; 1 0.9 0.7]);
asserts.isequal(stretchlim([0 1], [0.6 0.4]), [0; 1]);
B = imadjust([0 0.5 1], [0; 1], [1; 0]);
asserts.isapprox(B, [1 0.5 0], 1e-15);
MAP = [0 0.2 0.4; 0.5 0.6 0.7; 1 0.9 0.8];
MAP_ADJ = imadjust(MAP, [0 0.2 0.4; 1 0.9 0.8], [0 0 0; 1 1 1]);
asserts.isapprox(MAP_ADJ, [0 0 0; 0.5 4 / 7 0.75; 1 1 1], 1e-15);
MAP_DEFAULT = imadjust(MAP, [], [0 0 0; 1 1 1]);
asserts.isapprox(MAP_DEFAULT, [0 0 0; 0.5 4 / 7 0.75; 1 1 1], 1e-15);
asserts.isequal(class(imadjust(int16([-32768 32767]), [0; 1], [0; 1])), 'int16');
RGB = zeros(1, 2, 3);
RGB(:, :, 1) = [0 0.5];
RGB(:, :, 2) = [0.5 1];
RGB(:, :, 3) = [0 0.5];
ADJ = imadjust(RGB, [0 0.5 0; 1 1 0.5], [0 0 1; 1 1 0]);
REF_ADJ = zeros(1, 2, 3);
REF_ADJ(:, :, 1) = [0 0.5];
REF_ADJ(:, :, 2) = [0 1];
REF_ADJ(:, :, 3) = [1 0];
asserts.isapprox(ADJ, REF_ADJ, 1e-15);
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'thresholding') && fixtures.available.thresholding
  [LEVEL_REF, METRIC_REF] = graythresh([0 0 1 1]);
  asserts.isapprox(LEVEL_REF, fixtures.graythresh_level, 1e-12);
  asserts.isapprox(METRIC_REF, fixtures.graythresh_metric, 1e-12);
  THRESH_INPUT = [0.1 0.1 0.1; 0.1 0.9 0.1; 0.1 0.1 0.1];
  asserts.isapprox(adaptthresh(THRESH_INPUT, 0.4, 'ForegroundPolarity', 'bright'), ...
    fixtures.adaptthresh_bright, 1e-12);
  asserts.isequal(imbinarize(THRESH_INPUT, 'adaptive', 'Sensitivity', 0.4), ...
    fixtures.imbinarize_adaptive);
end
if isfield(fixtures.available, 'watershed') && fixtures.available.watershed
  WS_INPUT = [1 2 3; 2 3 2; 3 2 1];
  WS_PLATEAU = [1 2 1; 2 2 2; 1 2 1];
  WS_C4 = logical([0 1 0; 1 1 1; 0 1 0]);
  asserts.isequal(watershed(WS_INPUT, 4), fixtures.watershed_4);
  asserts.isequal(watershed(WS_INPUT, 8), fixtures.watershed_8);
  asserts.isequal(watershed(WS_INPUT, WS_C4), fixtures.watershed_c4);
  if isfield(fixtures, 'watershed_single_c4')
    asserts.isequal(watershed(WS_INPUT, single(WS_C4)), fixtures.watershed_single_c4);
  end
  asserts.isequal(watershed(uint8(WS_INPUT), 4), fixtures.watershed_uint8_4);
  if isfield(fixtures, 'watershed_int8_4')
    asserts.isequal(watershed(int8(WS_INPUT), 4), fixtures.watershed_int8_4);
  end
  asserts.isequal(watershed(WS_PLATEAU, 4), fixtures.watershed_plateau_4);
end
if isfield(fixtures.available, 'watershed3d') && fixtures.available.watershed3d
  WS3_INPUT = ones(2, 2, 2);
  WS3_C6 = false(3, 3, 3);
  WS3_C6(2, 2, 2) = true;
  WS3_C6(1, 2, 2) = true; WS3_C6(3, 2, 2) = true;
  WS3_C6(2, 1, 2) = true; WS3_C6(2, 3, 2) = true;
  WS3_C6(2, 2, 1) = true; WS3_C6(2, 2, 3) = true;
  asserts.isequal(watershed(WS3_INPUT), fixtures.watershed3d_default);
  asserts.isequal(watershed(WS3_INPUT, 6), fixtures.watershed3d_6);
  asserts.isequal(watershed(WS3_INPUT, 18), fixtures.watershed3d_18);
  asserts.isequal(watershed(uint8(WS3_INPUT), 26), fixtures.watershed3d_26);
  asserts.isequal(watershed(WS3_INPUT, WS3_C6), fixtures.watershed3d_c6);
  if isfield(fixtures, 'watershed3d_single_c6')
    asserts.isequal(watershed(WS3_INPUT, single(WS3_C6)), fixtures.watershed3d_single_c6);
  end
end
if isfield(fixtures.available, 'activecontour') && fixtures.available.activecontour
  AC_INPUT = zeros(7, 7);
  AC_INPUT(3:5, 3:5) = 1;
  AC_MASK = false(7, 7);
  AC_MASK(4, 4) = true;
  AC_NUMERIC_MASK = double(AC_MASK);
  asserts.isequal(activecontour(AC_INPUT, AC_MASK, 8), fixtures.activecontour_default);
  if isfield(fixtures, 'activecontour_int8')
    asserts.isequal(activecontour(int8(AC_INPUT * 10), AC_MASK, 8), fixtures.activecontour_int8);
  end
  if isfield(fixtures, 'activecontour_default_smooth0')
    asserts.isequal(activecontour(AC_INPUT, AC_MASK, 8, 'SmoothFactor', 0), ...
      fixtures.activecontour_default_smooth0);
  end
  asserts.isequal(activecontour(AC_INPUT, AC_MASK, 8, 'edge'), fixtures.activecontour_edge);
  asserts.isequal(activecontour(single(AC_INPUT), AC_NUMERIC_MASK, 'Iterations', uint8(8), ...
    'Method', 'chan-vese', 'SmoothFactor', true, 'ContractionBias', int8(0)), ...
    fixtures.activecontour_name_value);
end
if isfield(fixtures.available, 'activecontour_rgb') && fixtures.available.activecontour_rgb
  AC_INPUT = zeros(7, 7);
  AC_INPUT(3:5, 3:5) = 1;
  AC_MASK = false(7, 7);
  AC_MASK(4, 4) = true;
  AC_RGB = cat(3, AC_INPUT, AC_INPUT, AC_INPUT);
  AC_RGBA = cat(3, AC_RGB, 0.5 * ones(size(AC_INPUT)));
  asserts.isequal(activecontour(AC_RGB, AC_MASK, 8), fixtures.activecontour_rgb);
  if isfield(fixtures, 'activecontour_rgba_default')
    asserts.isequal(activecontour(AC_RGBA, AC_MASK, 8), fixtures.activecontour_rgba_default);
  end
end
if isfield(fixtures.available, 'registration') && fixtures.available.registration
  I_REG = zeros(16, 16);
  I_REG(5:8, 6:9) = 1;
  J_REG = imtranslate(I_REG, [3 -2], 'nearest');
  RGB_REG = cat(3, I_REG, I_REG * 0.5, I_REG * 0.25);
  RGB_REG_SHIFT = cat(3, J_REG, J_REG * 0.5, J_REG * 0.25);
  [REG_CORR_REF, REG_PEAK_REF] = imregcorr(I_REG, J_REG, 'translation');
  asserts.isapprox(REG_CORR_REF.T, fixtures.imregcorr_translation_t, 1e-12);
  asserts.isapprox(REG_PEAK_REF, fixtures.imregcorr_peak, 1e-12);
  asserts.isapprox(imregcorr(RGB_REG, RGB_REG_SHIFT, 'translation').T, ...
    fixtures.imregcorr_rgb_t, 1e-12);
  [REG_OPT_REF, REG_MET_REF] = imregconfig('monomodal');
  asserts.isequal(REG_OPT_REF.Type, fixtures.imregconfig_optimizer_type);
  asserts.isequal(REG_MET_REF.Type, fixtures.imregconfig_metric_type);
  REG_TFORM_REF = imregtform(I_REG, J_REG, 'translation', REG_OPT_REF, REG_MET_REF, ...
    'Interpolation', 'nearest', 'PyramidLevels', 1, 'DisplayOptimization', false);
  asserts.isapprox(REG_TFORM_REF.T, fixtures.imregtform_translation_t, 1e-12);
  REG_IMAGE_REF = imregister(I_REG, J_REG, 'translation', REG_OPT_REF, REG_MET_REF, ...
    'Interpolation', 'nearest', 'PyramidLevels', 1, 'DisplayOptimization', false);
  asserts.isapprox(REG_IMAGE_REF, fixtures.imregister_translation, 1e-12);
end
if isfield(fixtures.available, 'geometry') && fixtures.available.geometry
  GEOM_INPUT = reshape(1:9, 3, 3);
  GEOM_REF = imref2d([3 4]);
  asserts.isequal(GEOM_REF.ImageSize, fixtures.imref2d_default.ImageSize);
  asserts.isapprox(GEOM_REF.XWorldLimits, fixtures.imref2d_default.XWorldLimits, 1e-12);
  asserts.isapprox(GEOM_REF.YWorldLimits, fixtures.imref2d_default.YWorldLimits, 1e-12);
  if isfield(fixtures, 'imref2d_pixel_xworldlimits')
    GEOM_PIXEL_REF = imref2d([2 3], 2, 3);
    asserts.isapprox(GEOM_PIXEL_REF.XWorldLimits, fixtures.imref2d_pixel_xworldlimits, 1e-12);
    asserts.isapprox(GEOM_PIXEL_REF.YWorldLimits, fixtures.imref2d_pixel_yworldlimits, 1e-12);
    asserts.isapprox(GEOM_PIXEL_REF.PixelExtentInWorldX, fixtures.imref2d_pixel_extent_x, 1e-12);
    asserts.isapprox(GEOM_PIXEL_REF.PixelExtentInWorldY, fixtures.imref2d_pixel_extent_y, 1e-12);
  end
  asserts.isequal(imwarp(GEOM_INPUT, affine2d(eye(3)), 'nearest'), fixtures.imwarp_identity);
  asserts.isequal(imwarp(GEOM_INPUT, affine2d(eye(3)), 'nearest', 'OutputView', GEOM_REF), ...
    fixtures.imwarp_ref);
  if isfield(fixtures, 'imwarp_source_ref')
    asserts.isequal(imwarp(GEOM_INPUT, fixtures.imref2d_source, affine2d(eye(3)), 'nearest'), ...
      fixtures.imwarp_source_ref);
    [~, SOURCE_WORLD] = imwarp(GEOM_INPUT, fixtures.imref2d_source, affine2d(eye(3)), 'nearest');
    asserts.isequal(SOURCE_WORLD.ImageSize, fixtures.imwarp_source_world.ImageSize);
    asserts.isapprox(SOURCE_WORLD.XWorldLimits, fixtures.imwarp_source_world.XWorldLimits, 1e-12);
    asserts.isapprox(SOURCE_WORLD.YWorldLimits, fixtures.imwarp_source_world.YWorldLimits, 1e-12);
  end
  if isfield(fixtures, 'intrinsicToWorld_2d_x')
    [GEOM_XW, GEOM_YW] = intrinsicToWorld(fixtures.imref2d_source, [1 3], [1 3]);
    asserts.isapprox(GEOM_XW, fixtures.intrinsicToWorld_2d_x, 1e-12);
    asserts.isapprox(GEOM_YW, fixtures.intrinsicToWorld_2d_y, 1e-12);
    [GEOM_XI, GEOM_YI] = worldToIntrinsic(fixtures.imref2d_source, GEOM_XW, GEOM_YW);
    asserts.isapprox(GEOM_XI, fixtures.worldToIntrinsic_2d_x, 1e-12);
    asserts.isapprox(GEOM_YI, fixtures.worldToIntrinsic_2d_y, 1e-12);
    [GEOM_ROW, GEOM_COL] = worldToSubscript(fixtures.imref2d_source, [11 15], [21 25]);
    asserts.isequal(GEOM_ROW, fixtures.worldToSubscript_2d_row);
    asserts.isequal(GEOM_COL, fixtures.worldToSubscript_2d_col);
    asserts.isequal(sizesMatch(fixtures.imref2d_source, zeros(3, 3, 3)), fixtures.sizesMatch_2d_rgb);
  end
end
if isfield(fixtures.available, 'geometry_extended') && fixtures.available.geometry_extended
  GEOM_INPUT = reshape(1:9, 3, 3);
  GEOM_RGB = zeros(2, 2, 3);
  GEOM_RGB(:, :, 1) = [1 2; 3 4];
  GEOM_RGB(:, :, 2) = [10 20; 30 40];
  GEOM_RGB(:, :, 3) = [100 200; 300 400];
  asserts.isequal(imwarp(uint8(GEOM_INPUT), affine2d(eye(3)), 'nearest', ...
    'OutputView', [4 5]), fixtures.imwarp_numeric_view_uint8);
  if isfield(fixtures, 'imwarp_int8_identity')
    asserts.isequal(imwarp(int8(GEOM_INPUT) - 5, affine2d(eye(3)), 'nearest'), ...
      fixtures.imwarp_int8_identity);
  end
  asserts.isapprox(imwarp(GEOM_INPUT, [1 0 0; 0 1 0; 0.5 0 1], ...
    'linear', 'FillValues', -1, 'OutputView', 'same'), fixtures.imwarp_linear_fill, 1e-12);
  asserts.isequal(imwarp(GEOM_RGB, [1 0 0; 0 1 0; 1 0 1], ...
    'nearest', 'FillValues', [4 5 6], 'OutputView', 'same'), fixtures.imwarp_rgb_fill);
  GEOM_SRC_REF = imref2d([3 3], [10.5 13.5], [20.5 23.5]);
  [GEOM_FULL, GEOM_FULL_REF] = imwarp(GEOM_INPUT, GEOM_SRC_REF, ...
    affine2d([1 0 0; 0 1 0; 1 0 1]), 'nearest', 'OutputView', 'full');
  asserts.isequal(GEOM_FULL, fixtures.imwarp_source_full);
  asserts.isequal(GEOM_FULL_REF.ImageSize, fixtures.imwarp_source_full_ref.ImageSize);
  asserts.isapprox(GEOM_FULL_REF.XWorldLimits, fixtures.imwarp_source_full_ref.XWorldLimits, 1e-12);
  asserts.isapprox(GEOM_FULL_REF.YWorldLimits, fixtures.imwarp_source_full_ref.YWorldLimits, 1e-12);
end
if isfield(fixtures.available, 'geometry_rgba') && fixtures.available.geometry_rgba
  GEOM_RGBA = zeros(2, 2, 4);
  GEOM_RGBA(:, :, 1) = [1 2; 3 4];
  GEOM_RGBA(:, :, 2) = [10 20; 30 40];
  GEOM_RGBA(:, :, 3) = [100 200; 300 400];
  GEOM_RGBA(:, :, 4) = [0.1 0.2; 0.3 0.4];
  asserts.isequal(imwarp(GEOM_RGBA, [1 0 0; 0 1 0; 1 0 1], ...
    'nearest', 'FillValues', [4 5 6 7], 'OutputView', 'same'), fixtures.imwarp_rgba_fill);
  asserts.isequal(imwarp(uint8(10 * GEOM_RGBA), affine2d(eye(3)), 'nearest'), ...
    fixtures.imwarp_rgba_uint8_identity);
end
if isfield(fixtures.available, 'geometry3d') && fixtures.available.geometry3d
  GEOM3_INPUT = reshape(1:27, [3 3 3]);
  GEOM3_REF = imref3d([3 4 5]);
  asserts.isequal(GEOM3_REF.ImageSize, fixtures.imref3d_imagesize);
  asserts.isapprox(GEOM3_REF.XWorldLimits, fixtures.imref3d_xworldlimits, 1e-12);
  asserts.isapprox(GEOM3_REF.YWorldLimits, fixtures.imref3d_yworldlimits, 1e-12);
  asserts.isapprox(GEOM3_REF.ZWorldLimits, fixtures.imref3d_zworldlimits, 1e-12);
  if isfield(fixtures, 'imref3d_pixel_xworldlimits')
    GEOM3_PIXEL_REF = imref3d([2 3 4], 2, 3, 4);
    asserts.isapprox(GEOM3_PIXEL_REF.XWorldLimits, fixtures.imref3d_pixel_xworldlimits, 1e-12);
    asserts.isapprox(GEOM3_PIXEL_REF.YWorldLimits, fixtures.imref3d_pixel_yworldlimits, 1e-12);
    asserts.isapprox(GEOM3_PIXEL_REF.ZWorldLimits, fixtures.imref3d_pixel_zworldlimits, 1e-12);
    asserts.isapprox(GEOM3_PIXEL_REF.PixelExtentInWorldX, fixtures.imref3d_pixel_extent_x, 1e-12);
    asserts.isapprox(GEOM3_PIXEL_REF.PixelExtentInWorldY, fixtures.imref3d_pixel_extent_y, 1e-12);
    asserts.isapprox(GEOM3_PIXEL_REF.PixelExtentInWorldZ, fixtures.imref3d_pixel_extent_z, 1e-12);
  end
  asserts.isequal(affine3d(eye(4)).T, fixtures.affine3d_identity_t);
  asserts.isequal(imwarp(GEOM3_INPUT, affine3d(eye(4)), 'nearest'), ...
    fixtures.imwarp3d_identity);
  if isfield(fixtures, 'imwarp3d_numeric_3x4_identity')
    asserts.isequal(imwarp(GEOM3_INPUT, [1 0 0 0; 0 1 0 0; 0 0 1 0], 'nearest'), ...
      fixtures.imwarp3d_numeric_3x4_identity);
  end
  if isfield(fixtures, 'imwarp3d_string_options')
    asserts.isequal(imwarp(GEOM3_INPUT, affine3d(eye(4)), string('Interpolation'), string('Bilinear'), ...
      string('OutputView'), uint8([2 2 2]), string('FillValues'), int8(-1)), ...
      fixtures.imwarp3d_string_options);
  end
  asserts.isequal(imwarp(uint8(GEOM3_INPUT), affine3d(eye(4)), 'nearest'), ...
    fixtures.imwarp3d_uint8_identity);
  if isfield(fixtures, 'imwarp3d_int8_identity')
    asserts.isequal(imwarp(int8(GEOM3_INPUT) - 14, affine3d(eye(4)), 'nearest'), ...
      fixtures.imwarp3d_int8_identity);
  end
  if isfield(fixtures, 'imwarp3d_complex_identity')
    GEOM3_COMPLEX = cat(3, [1 + i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
    asserts.isequal(imwarp(GEOM3_COMPLEX, affine3d(eye(4)), 'nearest'), ...
      fixtures.imwarp3d_complex_identity);
  end
  GEOM3_SRC_REF = imref3d([3 3 3], [10.5 13.5], [20.5 23.5], [30.5 33.5]);
  [GEOM3_SOURCE, GEOM3_SOURCE_WORLD] = imwarp(GEOM3_INPUT, GEOM3_SRC_REF, ...
    affine3d(eye(4)), 'nearest');
  asserts.isequal(GEOM3_SOURCE, fixtures.imwarp3d_source_ref);
  asserts.isequal(GEOM3_SOURCE_WORLD.ImageSize, fixtures.imwarp3d_source_world_imagesize);
  asserts.isapprox(GEOM3_SOURCE_WORLD.XWorldLimits, fixtures.imwarp3d_source_world_xlimits, 1e-12);
  asserts.isapprox(GEOM3_SOURCE_WORLD.YWorldLimits, fixtures.imwarp3d_source_world_ylimits, 1e-12);
  asserts.isapprox(GEOM3_SOURCE_WORLD.ZWorldLimits, fixtures.imwarp3d_source_world_zlimits, 1e-12);
  if isfield(fixtures, 'intrinsicToWorld_3d_x')
    [GEOM3_XW, GEOM3_YW, GEOM3_ZW] = intrinsicToWorld(GEOM3_SRC_REF, [1 3], [1 3], [1 3]);
    asserts.isapprox(GEOM3_XW, fixtures.intrinsicToWorld_3d_x, 1e-12);
    asserts.isapprox(GEOM3_YW, fixtures.intrinsicToWorld_3d_y, 1e-12);
    asserts.isapprox(GEOM3_ZW, fixtures.intrinsicToWorld_3d_z, 1e-12);
    [GEOM3_XI, GEOM3_YI, GEOM3_ZI] = worldToIntrinsic(GEOM3_SRC_REF, ...
      GEOM3_XW, GEOM3_YW, GEOM3_ZW);
    asserts.isapprox(GEOM3_XI, fixtures.worldToIntrinsic_3d_x, 1e-12);
    asserts.isapprox(GEOM3_YI, fixtures.worldToIntrinsic_3d_y, 1e-12);
    asserts.isapprox(GEOM3_ZI, fixtures.worldToIntrinsic_3d_z, 1e-12);
    [GEOM3_ROW, GEOM3_COL, GEOM3_PLANE] = worldToSubscript(GEOM3_SRC_REF, ...
      [11 15], [21 25], [31 35]);
    asserts.isequal(GEOM3_ROW, fixtures.worldToSubscript_3d_row);
    asserts.isequal(GEOM3_COL, fixtures.worldToSubscript_3d_col);
    asserts.isequal(GEOM3_PLANE, fixtures.worldToSubscript_3d_plane);
    asserts.isequal(sizesMatch(GEOM3_SRC_REF, zeros(3, 3, 3)), fixtures.sizesMatch_3d);
  end
  asserts.isequal(imwarp(GEOM3_INPUT, affine3d(eye(4)), 'nearest', 'OutputView', [4 4 4]), ...
    fixtures.imwarp3d_numeric_view);
end
