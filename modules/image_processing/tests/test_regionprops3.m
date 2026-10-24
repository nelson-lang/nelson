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
BW = false(5, 6, 4);
BW(3:4, 2:4, 3) = true;
BW(1, 6, 1) = true;
S = regionprops3(BW, 'Volume', 'Centroid', 'BoundingBox', 'VoxelIdxList', ...
  'VoxelList', 'Image', 'SubarrayIdx', 'Extent', 'EquivDiameter');
asserts.isequal(length(S), 2);
asserts.isequal(S(1).Volume, 1);
asserts.isequal(S(1).Centroid, [6 1 1]);
asserts.isequal(S(1).BoundingBox, [5.5 0.5 0.5 1 1 1]);
asserts.isequal(S(1).VoxelIdxList, 26);
asserts.isequal(S(1).VoxelList, [6 1 1]);
asserts.isequal(S(1).Image, true(1, 1, 1));
asserts.isequal(S(1).SubarrayIdx, {1, 6, 1});
asserts.isequal(S(1).Extent, 1);
asserts.isapprox(S(1).EquivDiameter, 2 * (3 / (4 * pi)) ^ (1 / 3), 1e-14);
asserts.isequal(S(2).Volume, 6);
asserts.isequal(S(2).Centroid, [3 3.5 3]);
asserts.isequal(S(2).BoundingBox, [1.5 2.5 2.5 3 2 1]);
asserts.isequal(size(S(2).Image), [2 3]);
asserts.isequal(S(2).SubarrayIdx, {3:4, 2:4, 3});
asserts.isequal(S(2).Extent, 1);
%=============================================================================
L = zeros(3, 4, 2);
L(1:2, 2, 1) = 5;
L(3, 4, 2) = 9;
SL = regionprops3(L, 'basic');
asserts.isequal(length(SL), 2);
asserts.isequal(SL(1).Volume, 2);
asserts.isequal(SL(1).Centroid, [2 1.5 1]);
asserts.isequal(SL(2).BoundingBox, [3.5 2.5 1.5 1 1 1]);
%=============================================================================
CC = bwconncomp(BW, 26);
SCC = regionprops3(CC, {'Volume', 'Centroid'});
asserts.isequal([SCC.Volume], [1 6]);
asserts.isequal(SCC(2).Centroid, S(2).Centroid);
%=============================================================================
I = reshape(1:numel(BW), size(BW));
SI = regionprops3(BW, I, 'Volume', 'MeanIntensity', 'MinIntensity', 'MaxIntensity', ...
  'VoxelValues', 'WeightedCentroid');
asserts.isequal(SI(1).MeanIntensity, 26);
asserts.isequal(SI(1).MinIntensity, 26);
asserts.isequal(SI(1).MaxIntensity, 26);
asserts.isequal(SI(1).VoxelValues, 26);
idx = S(2).VoxelIdxList;
[r, c, z] = ind2sub(size(BW), idx);
values = I(idx);
asserts.isequal(SI(2).MeanIntensity, mean(values));
asserts.isequal(SI(2).MinIntensity, min(values));
asserts.isequal(SI(2).MaxIntensity, max(values));
asserts.isequal(SI(2).VoxelValues, values(:));
refWeighted = [sum(c .* values) / sum(values), sum(r .* values) / sum(values), ...
  sum(z .* values) / sum(values)];
asserts.isapprox(SI(2).WeightedCentroid, refWeighted, 1e-14);
SZERO = regionprops3(BW, zeros(size(BW)), 'WeightedCentroid');
asserts.istrue(all(isnan(SZERO(1).WeightedCentroid)));
%=============================================================================
SEMPTY = regionprops3(false(2, 2, 2), 'Volume', 'Centroid');
asserts.isequal(length(SEMPTY), 0);
asserts.istrue(isfield(SEMPTY, 'Volume'));
asserts.istrue(isfield(SEMPTY, 'Centroid'));
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'regionprops3') && fixtures.available.regionprops3
  S_REF = regionprops3(BW, 'Volume', 'Centroid', 'BoundingBox', 'Extent', 'EquivDiameter');
  VOLUME_REF = [S_REF.Volume]';
  CENTROID_REF = zeros(length(S_REF), 3);
  BOUNDINGBOX_REF = zeros(length(S_REF), 6);
  EXTENT_REF = [S_REF.Extent]';
  EQUIVDIAMETER_REF = [S_REF.EquivDiameter]';
  for k = 1:length(S_REF)
    CENTROID_REF(k, :) = S_REF(k).Centroid;
    BOUNDINGBOX_REF(k, :) = S_REF(k).BoundingBox;
  end
  asserts.isapprox(VOLUME_REF, fixtures.regionprops3_volume, 1e-12);
  asserts.isapprox(CENTROID_REF, fixtures.regionprops3_centroid, 1e-12);
  asserts.isapprox(BOUNDINGBOX_REF, fixtures.regionprops3_boundingbox, 1e-12);
  asserts.isapprox(EXTENT_REF, fixtures.regionprops3_extent, 1e-12);
  asserts.isapprox(EQUIVDIAMETER_REF, fixtures.regionprops3_equivdiameter, 1e-12);
  SI_REF = regionprops3(BW, I, 'MeanIntensity', 'MinIntensity', 'MaxIntensity', ...
    'WeightedCentroid');
  WEIGHTED_REF = zeros(length(SI_REF), 3);
  for k = 1:length(SI_REF)
    WEIGHTED_REF(k, :) = SI_REF(k).WeightedCentroid;
  end
  asserts.isapprox([SI_REF.MeanIntensity]', fixtures.regionprops3_meanintensity, 1e-12);
  asserts.isapprox([SI_REF.MinIntensity]', fixtures.regionprops3_minintensity, 1e-12);
  asserts.isapprox([SI_REF.MaxIntensity]', fixtures.regionprops3_maxintensity, 1e-12);
  asserts.isapprox(WEIGHTED_REF, fixtures.regionprops3_weightedcentroid, 1e-12);
end
%=============================================================================
asserts.checkerror('regionprops3([1 0], ''Volume'')', sprintf(_('%s must be a 3-D array.'), 'Volume'));
%=============================================================================
asserts.checkerror('regionprops3(cat(3, [1 0], [1 + i 0]), ''Volume'')', sprintf(_('%s must be real.'), 'Volume'));
%=============================================================================
asserts.checkerror('regionprops3(sparse([1 0]), ''Volume'')', sprintf(_('%s are not supported.'), 'Sparse volumes'));
%=============================================================================
asserts.checkerror('regionprops3(cat(3, [1.5 0], [0 0]), ''Volume'')', sprintf(_('%s values must be nonnegative finite integers.'), 'Label volume'));
%=============================================================================
asserts.checkerror('regionprops3(true(2, 2, 2), ''MeanIntensity'')', sprintf(_('%s is required.'), 'Intensity volume'));
%=============================================================================
asserts.checkerror('regionprops3(true(2, 2, 2), ones(2, 2, 3), ''MeanIntensity'')', sprintf(_('%s must have the same size as %s.'), 'Intensity volume', 'the region volume'));
%=============================================================================
asserts.checkerror('regionprops3(true(2, 2, 2), ''UnsupportedProperty'')', sprintf(_('Unsupported "%s" option.'), 'region property'));
