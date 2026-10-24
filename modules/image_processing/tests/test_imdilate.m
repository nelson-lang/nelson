%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
BW = false(7, 15);
BW(2:6, 3:4) = true;
BW(2:6, 8:9) = true;
BW(4, 5:7) = true;
J = imdilate(BW, strel('square', 3));
REF = false(7, 15);
REF(:, 2:5) = true;
REF(3:5, 6) = true;
REF(3:5, 7) = true;
REF(:, 7:10) = REF(:, 7:10) | true(7, 4);
asserts.isequal(J, REF);
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'morphology') && fixtures.available.morphology
  MORPH_INPUT = logical([0 1 0; 0 1 0; 0 0 0]);
  asserts.isequal(imdilate(MORPH_INPUT, strel('square', 3)), fixtures.imdilate_square);
  asserts.isequal(imerode(true(3, 3), strel('square', 3)), fixtures.imerode_square);
  asserts.isequal(bwperim(true(3, 3)), fixtures.bwperim_square);
  if isfield(fixtures, 'bwperim_single_square')
    asserts.isequal(bwperim(single(true(3, 3))), fixtures.bwperim_single_square);
  end
end
if isfield(fixtures.available, 'bwmorph') && fixtures.available.bwmorph
  DILATE_SEED = false(5, 5);
  DILATE_SEED(3, 3) = true;
  OPEN_INPUT = false(5, 5);
  OPEN_INPUT(2:4, 2:4) = true;
  OPEN_INPUT(1, 1) = true;
  CLOSE_INPUT = false(5, 5);
  CLOSE_INPUT(2:4, 2:4) = true;
  CLOSE_INPUT(3, 3) = false;
  asserts.isequal(bwmorph(DILATE_SEED, 'dilate'), fixtures.bwmorph_dilate);
  asserts.isequal(bwmorph(DILATE_SEED, 'dilate', 2), fixtures.bwmorph_dilate_2);
  asserts.isequal(bwmorph(true(5, 5), 'erode'), fixtures.bwmorph_erode);
  asserts.isequal(bwmorph(OPEN_INPUT, 'open'), fixtures.bwmorph_open);
  asserts.isequal(bwmorph(CLOSE_INPUT, 'close'), fixtures.bwmorph_close);
  asserts.isequal(bwmorph(OPEN_INPUT, 'tophat'), fixtures.bwmorph_tophat);
  asserts.isequal(bwmorph(CLOSE_INPUT, 'bothat'), fixtures.bwmorph_bothat);
  if isfield(fixtures, 'bwmorph_int8_clean')
    asserts.isequal(bwmorph(int8([1 0; 0 1]), 'clean'), fixtures.bwmorph_int8_clean);
  end
end
if isfield(fixtures.available, 'bwconncomp3') && fixtures.available.bwconncomp3
  BW3_FIXTURE = false(3, 3, 3);
  BW3_FIXTURE(2, 2, 1) = true;
  BW3_FIXTURE(2, 2, 2) = true;
  BW3_FIXTURE(1, 1, 3) = true;
  CC3_6_REF = bwconncomp(BW3_FIXTURE, 6);
  CC3_26_REF = bwconncomp(BW3_FIXTURE, 26);
  asserts.isequal(CC3_6_REF.NumObjects, fixtures.bwconncomp3_6_numobjects);
  asserts.isequal(labelmatrix(CC3_6_REF), fixtures.bwconncomp3_6_labelmatrix);
  asserts.isequal(CC3_26_REF.NumObjects, fixtures.bwconncomp3_26_numobjects);
  asserts.isequal(labelmatrix(CC3_26_REF), fixtures.bwconncomp3_26_labelmatrix);
end
if isfield(fixtures.available, 'regionprops') && fixtures.available.regionprops
  REGION_INPUT = logical([1 0 0 1; 1 0 0 0; 0 0 1 1]);
  REGION_INTENSITY = [10 0 0 2; 20 0 0 0; 0 0 30 40];
  S_REF = regionprops(REGION_INPUT, REGION_INTENSITY, 'Area', 'Centroid', 'BoundingBox', 'MeanIntensity');
  asserts.isequal(length(S_REF), length(fixtures.regionprops_basic));
  for k = 1:length(S_REF)
    asserts.isequal(S_REF(k).Area, fixtures.regionprops_basic(k).Area);
    asserts.isapprox(S_REF(k).Centroid, fixtures.regionprops_basic(k).Centroid, 1e-12);
    asserts.isapprox(S_REF(k).BoundingBox, fixtures.regionprops_basic(k).BoundingBox, 1e-12);
    asserts.isapprox(S_REF(k).MeanIntensity, fixtures.regionprops_basic(k).MeanIntensity, 1e-12);
  end
  AXIS_REF = regionprops(logical([1 1 1]), 'Orientation', ...
    'MajorAxisLength', 'MinorAxisLength', 'Eccentricity');
  asserts.isapprox(AXIS_REF.Orientation, fixtures.regionprops_axis.Orientation, 1e-12);
  asserts.isapprox(AXIS_REF.MajorAxisLength, fixtures.regionprops_axis.MajorAxisLength, 1e-12);
  asserts.isapprox(AXIS_REF.MinorAxisLength, fixtures.regionprops_axis.MinorAxisLength, 1e-12);
  asserts.isapprox(AXIS_REF.Eccentricity, fixtures.regionprops_axis.Eccentricity, 1e-12);
end
%=============================================================================
asserts.checkerror('imdilate([1 2], false(2, 2))', _('Structuring element neighborhood must contain at least one true value.'));
%=============================================================================
asserts.checkerror('imdilate([1 2], strel(''cube'', 3))', sprintf(_('%s must be a 3-D array for 3-D structuring elements.'), 'Image'));
