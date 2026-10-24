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
R3D = imref3d([3 4 5]);
asserts.isequal(R3D.Type, 'imref3d');
asserts.isequal(R3D.ImageSize, [3 4 5]);
asserts.isequal(R3D.XWorldLimits, [0.5 4.5]);
asserts.isequal(R3D.YWorldLimits, [0.5 3.5]);
asserts.isequal(R3D.ZWorldLimits, [0.5 5.5]);
asserts.isequal(R3D.XIntrinsicLimits, [0.5 4.5]);
asserts.isequal(R3D.YIntrinsicLimits, [0.5 3.5]);
asserts.isequal(R3D.ZIntrinsicLimits, [0.5 5.5]);
asserts.isequal(R3D.PixelExtentInWorldX, 1);
asserts.isequal(R3D.PixelExtentInWorldY, 1);
asserts.isequal(R3D.PixelExtentInWorldZ, 1);
R3D_DEFAULT = imref3d();
asserts.isequal(R3D_DEFAULT.ImageSize, [2 2 2]);
asserts.isequal(R3D_DEFAULT.ZWorldLimits, [0.5 2.5]);
R3D_PIXEL = imref3d([2 3 4], 2, 3, 4);
asserts.isequal(R3D_PIXEL.XWorldLimits, [1 7]);
asserts.isequal(R3D_PIXEL.YWorldLimits, [1.5 7.5]);
asserts.isequal(R3D_PIXEL.ZWorldLimits, [2 18]);
asserts.isequal(R3D_PIXEL.PixelExtentInWorldX, 2);
asserts.isequal(R3D_PIXEL.PixelExtentInWorldY, 3);
asserts.isequal(R3D_PIXEL.PixelExtentInWorldZ, 4);
[XW3, YW3, ZW3] = intrinsicToWorld(R3D_PIXEL, [1 3], [1 2], [1 4]);
asserts.isequal(XW3, [2 6]);
asserts.isequal(YW3, [3 6]);
asserts.isequal(ZW3, [4 16]);
[XI3, YI3, ZI3] = worldToIntrinsic(R3D_PIXEL, XW3, YW3, ZW3);
asserts.isequal(XI3, [1 3]);
asserts.isequal(YI3, [1 2]);
asserts.isequal(ZI3, [1 4]);
[RS3, CS3, PS3] = worldToSubscript(R3D_PIXEL, [2 6 8], [3 6 6], [4 16 4]);
asserts.isequal(RS3(1:2), [1 2]);
asserts.isequal(CS3(1:2), [1 3]);
asserts.isequal(PS3(1:2), [1 4]);
asserts.istrue(isnan(RS3(3)) && isnan(CS3(3)) && isnan(PS3(3)));
asserts.istrue(sizesMatch(R3D_PIXEL, zeros(2, 3, 4)));
asserts.isfalse(sizesMatch(R3D_PIXEL, zeros(2, 3, 3)));
R3D_WORLD = imref3d([2 3 4], [10 16], [20 24], [30 38]);
asserts.isequal(R3D_WORLD.PixelExtentInWorldX, 2);
asserts.isequal(R3D_WORLD.PixelExtentInWorldY, 2);
asserts.isequal(R3D_WORLD.PixelExtentInWorldZ, 2);
TA3 = affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 4 5 6 1]);
asserts.isequal(TA3.Type, 'affine');
asserts.isequal(TA3.Dimensionality, 3);
asserts.isequal(TA3.T, [1 0 0 0; 0 1 0 0; 0 0 1 0; 4 5 6 1]);
asserts.isequal(affine3d().T, eye(4));
V3W = reshape(1:27, [3 3 3]);
[V3_ID, V3_R] = imwarp(V3W, affine3d(eye(4)), 'nearest');
asserts.isequal(V3_ID, V3W);
asserts.isequal(V3_R.ImageSize, [3 3 3]);
V3_NUMERIC_3X4 = imwarp(V3W, [1 0 0 0; 0 1 0 0; 0 0 1 0], 'nearest');
asserts.isequal(V3_NUMERIC_3X4, V3W);
V3_STRING_OPTIONS = imwarp(V3W, affine3d(eye(4)), string('Interpolation'), string('Bilinear'), ...
  string('OutputView'), uint8([3 3 3]), string('FillValues'), int8(-1));
asserts.isequal(V3_STRING_OPTIONS, V3W);
V3_SHIFT = imwarp(V3W, affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 1 0 0 1]), ...
  'nearest', 'FillValues', -1, 'OutputView', 'same');
asserts.isequal(V3_SHIFT(:, 1, :), -ones(3, 1, 3));
asserts.isequal(V3_SHIFT(:, 2:3, :), V3W(:, 1:2, :));
asserts.isequal(imwarp(V3W, affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 1 0 0 1]), ...
  'nearest'), V3W);
V3_LINEAR = imwarp(V3W, [1 0 0 0; 0 1 0 0; 0 0 1 0; 0.5 0 0 1], ...
  'linear', 'FillValues', -1, 'OutputView', 'same');
asserts.isapprox(V3_LINEAR(1, 2, 1), 2.5, 1e-14);
V3_UINT8 = imwarp(uint8(V3W), affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_UINT8), 'uint8');
asserts.isequal(V3_UINT8, uint8(V3W));
V3_UINT16 = imwarp(uint16(V3W), affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_UINT16), 'uint16');
asserts.isequal(V3_UINT16, uint16(V3W));
V3_INT8 = imwarp(int8(V3W) - 14, affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_INT8), 'int8');
asserts.isequal(V3_INT8, int8(V3W) - 14);
V3_UINT32 = imwarp(uint32(V3W), affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_UINT32), 'uint32');
asserts.isequal(V3_UINT32, uint32(V3W));
V3_UINT64 = imwarp(uint64(V3W), affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_UINT64), 'uint64');
asserts.isequal(V3_UINT64, uint64(V3W));
V3_INT32 = imwarp(int32(V3W), affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_INT32), 'int32');
asserts.isequal(V3_INT32, int32(V3W));
V3_INT64 = imwarp(int64(V3W), affine3d(eye(4)), 'nearest');
asserts.isequal(class(V3_INT64), 'int64');
asserts.isequal(V3_INT64, int64(V3W));
V3_SINGLE = imwarp(single(V3W), affine3d(eye(4)), 'linear');
asserts.isequal(class(V3_SINGLE), 'single');
asserts.isequal(V3_SINGLE, single(V3W));
V3_LINEAR_ALIAS = imwarp(single(V3W), affine3d(eye(4)), 'Interpolation', 'Bilinear');
asserts.isequal(V3_LINEAR_ALIAS, V3_SINGLE);
V3_NAN = imwarp(cat(3, [1 2; 3 NaN], [5 6; 7 8]), affine3d(eye(4)), 'nearest');
asserts.istrue(isnan(V3_NAN(2, 2, 1)));
V3_INF = imwarp(cat(3, [1 2; 3 Inf], [5 6; 7 8]), affine3d(eye(4)), 'nearest');
asserts.istrue(isinf(V3_INF(2, 2, 1)));
V3_COMPLEX = cat(3, [1 + i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
V3_COMPLEX_WARP = imwarp(V3_COMPLEX, affine3d(eye(4)), 'nearest');
asserts.isequal(V3_COMPLEX_WARP, V3_COMPLEX);
R3SRC = imref3d([3 3 3], [10.5 13.5], [20.5 23.5], [30.5 33.5]);
[V3_SRC, R3_OUT] = imwarp(V3W, R3SRC, affine3d(eye(4)), 'nearest');
asserts.isequal(V3_SRC, V3W);
asserts.isequal(R3_OUT.XWorldLimits, [10.5 13.5]);
R3OUT = imref3d([4 4 4]);
[V3_OV, R3_OV] = imwarp(V3W, affine3d(eye(4)), 'nearest', 'OutputView', R3OUT);
asserts.isequal(size(V3_OV), [4 4 4]);
asserts.isequal(R3_OV.ImageSize, [4 4 4]);
V3_NUMERIC_VIEW = imwarp(V3W, affine3d(eye(4)), 'nearest', 'OutputView', [4 4 4]);
asserts.isequal(size(V3_NUMERIC_VIEW), [4 4 4]);
V3_LOGICAL_FILL = imwarp(logical(V3W > 20), affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 1 0 0 1]), ...
  'nearest', 'FillValues', true, 'OutputView', 'same');
asserts.isequal(class(V3_LOGICAL_FILL), 'logical');
V3_LOGICAL_FILL_COLUMN = V3_LOGICAL_FILL(:, 1, :);
asserts.istrue(all(V3_LOGICAL_FILL_COLUMN(:)));
%=============================================================================
