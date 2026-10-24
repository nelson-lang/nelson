%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
BW = false(10, 12);
BW(2:4, 2:5) = true;
BW(7:9, 8:10) = true;
S = regionprops(BW, 'Area', 'BoundingBox', 'Centroid');
asserts.isequal(length(S), 2);
asserts.isequal(S(1).Area, 12);
asserts.isequal(S(1).BoundingBox, [1.5 1.5 4 3]);
asserts.isapprox(S(1).Centroid, [3.5 3], 1e-14);
asserts.isequal(S(2).Area, 9);
asserts.isequal(S(2).BoundingBox, [7.5 6.5 3 3]);
asserts.isapprox(S(2).Centroid, [9 8], 1e-14);
%=============================================================================
BW2 = logical([1 0 0 1; 1 0 0 0; 0 0 1 1]);
I = [10 0 0 2; 20 0 0 0; 0 0 30 40];
S = regionprops(BW2, I, 'Area', 'MeanIntensity', 'MinIntensity', 'MaxIntensity', ...
  'PixelList', 'PixelValues', 'WeightedCentroid', 'Extent', 'EquivDiameter', 'Perimeter', 'SubarrayIdx');
asserts.isequal(S(1).Area, 2);
asserts.isequal(S(1).MeanIntensity, 15);
asserts.isequal(S(1).MinIntensity, 10);
asserts.isequal(S(1).MaxIntensity, 20);
asserts.isequal(S(1).PixelList, [1 1; 1 2]);
asserts.isequal(S(1).PixelValues, [10; 20]);
asserts.isapprox(S(1).WeightedCentroid, [1 5 / 3], 1e-15);
asserts.isequal(S(1).Extent, 1);
asserts.isapprox(S(1).EquivDiameter, sqrt(8 / pi), 1e-15);
asserts.isequal(S(1).Perimeter, 2);
asserts.isequal(S(1).SubarrayIdx, {1:2, 1});
SL = regionprops([1 0; 0 1], 'Area', 'BoundingBox');
asserts.isequal(length(SL), 1);
asserts.isequal(SL(1).Area, 2);
asserts.isequal(SL(1).BoundingBox, [0.5 0.5 2 2]);
SB = regionprops(logical([1 0; 0 1]), 'Area');
asserts.isequal(length(SB), 1);
SL2 = regionprops([1 0; 0 2], 'Area');
asserts.isequal(length(SL2), 2);
S_CASE = regionprops(BW2, 'centroid');
asserts.istrue(isfield(S_CASE, 'Centroid'));
S_BASIC = regionprops(BW2, 'basic');
asserts.istrue(isfield(S_BASIC, 'Area'));
asserts.istrue(isfield(S_BASIC, 'Centroid'));
asserts.istrue(isfield(S_BASIC, 'BoundingBox'));
%=============================================================================
asserts.checkerror('regionprops(logical([1 0]), ''MeanIntensity'')', sprintf(_('%s is required.'), 'Intensity image'));
%=============================================================================
asserts.checkerror('regionprops(logical([1 0]), ''PixelValues'')', sprintf(_('%s is required.'), 'Intensity image'));
%=============================================================================
asserts.checkerror('regionprops(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('regionprops(zeros(2, 2, 2))', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('regionprops([1.5 0])', sprintf(_('%s values must be nonnegative finite integers.'), 'Label matrix'));
%=============================================================================
asserts.checkerror('regionprops([1 0], [1; 2], ''MeanIntensity'')', sprintf(_('%s must have the same size as %s.'), 'Intensity image', 'the region image'));
%=============================================================================
asserts.checkerror('regionprops(logical([1 0]), ''UnsupportedProperty'')', sprintf(_('Unsupported "%s" option.'), 'region property'));
