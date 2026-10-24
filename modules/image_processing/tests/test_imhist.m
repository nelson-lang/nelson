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
[counts, bins] = imhist([0 0.5 1], 3);
asserts.isequal(counts, [1; 1; 1]);
asserts.isequal(bins, [0; 0.5; 1]);
asserts.isequal(imhist(uint16([0 65535]), 2), [1; 1]);
[~, bins16] = imhist(uint16([0 65535]), 2);
asserts.isequal(bins16, [0; 65535]);
BW = edge([0 0 0; 0 1 1; 0 1 1], 'roberts', 0.1);
asserts.istrue(islogical(BW));
I_CORNER = zeros(32, 32);
I_CORNER(10:22, 10:22) = 1;
M_HARRIS = cornermetric(I_CORNER);
M_MIN = cornermetric(I_CORNER, 'Minimum Eigenvalue');
M_MIN_STRING = cornermetric(I_CORNER, string('MinEigenValue'), string('FilterSize'), uint8(5), ...
  string('SensitivityFactor'), single(0.04));
asserts.isequal(size(M_HARRIS), size(I_CORNER));
asserts.isequal(size(M_MIN), size(I_CORNER));
asserts.isequal(size(M_MIN_STRING), size(I_CORNER));
asserts.istrue(max(M_HARRIS(:)) > 0);
P_HARRIS = detectHarrisFeatures(I_CORNER, 'MinQuality', 0.05);
asserts.isequal(P_HARRIS.Count, 4);
asserts.istrue(any(P_HARRIS.Location(:, 1) == 10 & P_HARRIS.Location(:, 2) == 10));
P_ROI = detectHarrisFeatures(I_CORNER, 'ROI', [8 8 6 6], 'MinQuality', 0.05);
asserts.isequal(P_ROI.Count, 1);
asserts.isequal(P_ROI.Location, [10 10]);
RGB_CORNER = zeros(32, 32, 3);
RGB_CORNER(:, :, 1) = I_CORNER;
RGB_CORNER(:, :, 2) = 0.5 * I_CORNER;
P_HARRIS_STRING = detectHarrisFeatures(RGB_CORNER, string('minquality'), single(0.05), ...
  string('filtersize'), uint8(5), string('ROI'), [8 8 18 18]);
asserts.isequal(P_HARRIS_STRING.Count, 4);
asserts.istrue(all(P_HARRIS_STRING.Metric(1:end - 1) >= P_HARRIS_STRING.Metric(2:end)));
P_FAST = detectFASTFeatures(I_CORNER, 'MinContrast', 0.2);
asserts.istrue(P_FAST.Count >= 4);
asserts.istrue(isfield(P_FAST, 'Metric'));
P_FAST_STRING = detectFASTFeatures(RGB_CORNER, string('MinContrast'), single(0.2), ...
  string('MinQuality'), 0, string('ROI'), [8 8 18 18]);
asserts.istrue(P_FAST_STRING.Count >= 4);
P_FAST_EMPTY = detectFASTFeatures(zeros(16, 16), 'MinContrast', 0.2);
asserts.isequal(P_FAST_EMPTY.Count, 0);
asserts.isequal(P_FAST_EMPTY.Location, zeros(0, 2));
asserts.isequal(P_FAST_EMPTY.Metric, zeros(0, 1));
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
load(referenceFile);
if isfield(fixtures.available, 'features') && fixtures.available.features
  asserts.isapprox(cornermetric(I_CORNER, 'Harris', 'FilterSize', 5), ...
    fixtures.cornermetric_harris, 1e-12);
  asserts.isapprox(cornermetric(RGB_CORNER, 'Minimum Eigenvalue', 'FilterSize', 5), ...
    fixtures.cornermetric_mineigen, 1e-12);
  P_HARRIS_REF = detectHarrisFeatures(I_CORNER, 'MinQuality', 0.05, 'ROI', [8 8 6 6]);
  asserts.isequal(P_HARRIS_REF.Count, fixtures.detect_harris_roi_count);
  asserts.isapprox(P_HARRIS_REF.Location, fixtures.detect_harris_roi_location, 1e-12);
  asserts.isapprox(P_HARRIS_REF.Metric, fixtures.detect_harris_roi_metric, 1e-12);
  P_FAST_REF = detectFASTFeatures(RGB_CORNER, 'MinContrast', 0.2, 'MinQuality', 0, ...
    'ROI', [8 8 18 18]);
  asserts.isequal(P_FAST_REF.Count, fixtures.detect_fast_rgb_count);
  asserts.isapprox(P_FAST_REF.Location, fixtures.detect_fast_rgb_location, 1e-12);
  asserts.isapprox(P_FAST_REF.Metric, fixtures.detect_fast_rgb_metric, 1e-12);
end
%=============================================================================
asserts.checkerror('imhist(zeros(2, 2, 3))', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('imhist([1 2], 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'Number of bins'));
%=============================================================================
asserts.checkerror('imhist(sparse([1 2]))', sprintf(_('%s are not supported.'), 'Sparse images'));
