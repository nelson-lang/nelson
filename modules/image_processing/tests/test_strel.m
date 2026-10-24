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
SE = strel('disk', 8);
asserts.isequal(size(SE.nhood), [17 17]);
asserts.istrue(SE.nhood(9, 9));
D = strel('diamond', 1);
asserts.isequal(D.nhood, logical([0 1 0; 1 1 1; 0 1 0]));
I = 0.2 + 0.6 * repmat(linspace(0, 1, 24), 16, 1);
limits = stretchlim(I);
J = imadjust(I, limits, [0; 1]);
asserts.istrue(min(J(:)) <= 0.01);
asserts.istrue(max(J(:)) >= 0.99);
%=============================================================================
BW = logical([0 1 0; 0 1 0; 0 0 0]);
SE = strel('square', 3);
asserts.isequal(strel('line', 3, 0).nhood, logical([1 1 1]));
asserts.isequal(strel('line', 3, 90).nhood, logical([1; 1; 1]));
asserts.isequal(strel([1 0; 0 1]).nhood, logical([1 0; 0 1]));
asserts.isequal(strel(single([1 0; 0 1])).nhood, logical([1 0; 0 1]));
asserts.isequal(strel('arbitrary', [0 1 0; 1 1 1; 0 1 0]).nhood, logical([0 1 0; 1 1 1; 0 1 0]));
asserts.isequal(strel('diamond', 1).nhood, logical([0 1 0; 1 1 1; 0 1 0]));
asserts.isequal(strel('disk', 1, 0).nhood, logical([0 1 0; 1 1 1; 0 1 0]));
asserts.isequal(strel('disk', 1, 4).nhood, strel('disk', 1).nhood);
asserts.isequal(strel('disk', 1, 6).nhood, strel('disk', 1).nhood);
asserts.isequal(strel('disk', 1, 8).nhood, strel('disk', 1).nhood);
OCT = strel('octagon', 3).nhood;
asserts.isequal(size(OCT), [7 7]);
asserts.isequal(OCT(1, :), logical([0 0 1 1 1 0 0]));
asserts.isequal(OCT(4, :), true(1, 7));
CUBE = strel('cube', 3).nhood;
asserts.isequal(size(CUBE), [3 3 3]);
asserts.isequal(sum(CUBE(:)), 27);
CUBOID = strel('cuboid', [1 2 3]).nhood;
asserts.isequal(size(CUBOID), [1 2 3]);
asserts.isequal(sum(CUBOID(:)), 6);
SPHERE = strel('sphere', 1).nhood;
asserts.isequal(size(SPHERE), [3 3 3]);
asserts.isequal(sum(SPHERE(:)), 7);
D = imdilate(BW, SE);
asserts.isequal(D, logical([1 1 1; 1 1 1; 1 1 1]));
E = imerode(D, SE);
asserts.isequal(E, logical([0 0 0; 0 1 0; 0 0 0]));
asserts.isequal(imopen(D, SE), D);
asserts.isequal(imclose(BW, SE), logical([0 0 0; 0 1 0; 0 0 0]));
P = bwperim(D);
asserts.isequal(P, logical([1 1 1; 1 0 1; 1 1 1]));
asserts.isequal(bwperim(single(D)), P);
RGB = zeros(2, 2, 3);
RGB(:, :, 1) = [1 2; 3 4];
RGB(:, :, 2) = [10 20; 30 40];
RGB(:, :, 3) = [100 200; 300 400];
D_RGB = imdilate(RGB, SE);
asserts.isequal(size(D_RGB), size(RGB));
asserts.isequal(D_RGB(:, :, 1), 4 * ones(2, 2));
asserts.isequal(D_RGB(:, :, 2), 40 * ones(2, 2));
asserts.isequal(D_RGB(:, :, 3), 400 * ones(2, 2));
E_RGB = imerode(RGB, SE);
asserts.isequal(E_RGB(:, :, 1), ones(2, 2));
asserts.isequal(E_RGB(:, :, 2), 10 * ones(2, 2));
asserts.isequal(E_RGB(:, :, 3), 100 * ones(2, 2));
INT_IMG = int16([1 2; 3 4]);
asserts.isequal(class(imdilate(INT_IMG, strel('square', 1))), 'int16');
asserts.isequal(imdilate(INT_IMG, strel('square', 1)), INT_IMG);
V3 = false(5, 5, 5);
V3(3, 3, 3) = true;
D3 = imdilate(V3, strel('sphere', 1));
REF_D3 = false(5, 5, 5);
REF_D3(3, 3, 3) = true;
REF_D3(2, 3, 3) = true;
REF_D3(4, 3, 3) = true;
REF_D3(3, 2, 3) = true;
REF_D3(3, 4, 3) = true;
REF_D3(3, 3, 2) = true;
REF_D3(3, 3, 4) = true;
asserts.isequal(D3, REF_D3);
asserts.isequal(imerode(D3, strel('sphere', 1)), V3);
V3_UINT8 = uint8(zeros(3, 3, 3));
V3_UINT8(2, 2, 2) = 10;
D3_UINT8 = imdilate(V3_UINT8, strel('cube', 3));
asserts.isequal(class(D3_UINT8), 'uint8');
asserts.isequal(D3_UINT8, uint8(10 * ones(3, 3, 3)));
%=============================================================================
asserts.checkerror('strel(''unknown'', 3)', sprintf(_('Unsupported "%s" option.'), 'structuring element shape'));
%=============================================================================
asserts.checkerror('strel(''square'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'Square size'));
%=============================================================================
asserts.checkerror('strel(''rectangle'', [2 0])', sprintf(_('%s size must contain positive integers.'), 'Rectangle'));
%=============================================================================
asserts.checkerror('strel(''line'', 1.5, 0)', sprintf(_('%s must be a positive integer scalar.'), 'Line length'));
%=============================================================================
asserts.checkerror('strel(''disk'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'Disk radius'));
%=============================================================================
asserts.checkerror('strel(''disk'', 1, 5)', _('Disk decomposition count must be 0, 4, 6, or 8.'));
%=============================================================================
asserts.checkerror('strel(''diamond'', [1 2])', sprintf(_('%s must be a nonnegative integer scalar.'), 'Diamond radius'));
%=============================================================================
asserts.checkerror('strel(''octagon'', 2)', _('Octagon radius must be a nonnegative multiple of 3.'));
%=============================================================================
asserts.checkerror('strel(''cube'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'Cube size'));
%=============================================================================
asserts.checkerror('strel(''cuboid'', [1 2])', sprintf(_('%s size must contain positive integers.'), 'Cuboid'));
%=============================================================================
asserts.checkerror('strel(''sphere'', -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'Sphere radius'));
%=============================================================================
asserts.checkerror('strel(''square'', 3, 1)', sprintf(_('%s structuring element requires %s.'), 'Square', 'a size'));
%=============================================================================
asserts.checkerror('strel(''arbitrary'')', sprintf(_('%s structuring element requires %s.'), 'Arbitrary', 'a neighborhood'));
%=============================================================================
asserts.checkerror('strel(false(2, 2))', _('Structuring element neighborhood must contain at least one true value.'));
