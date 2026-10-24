%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
A = [1 2; 3 4];
S = imboxfilt(A, [2 2], 'Padding', 0, 'NormalizationFactor', 1);
asserts.isequal(S, [1 3; 4 10]);
B = imgaussfilt(A, 0.5, 'FilterSize', [3 3], 'Padding', 0);
asserts.isapprox(B, [1.0838 1.6918; 2.2998 2.9078], 1e-4);
V = zeros(21, 21, 9);
V(8:14, 8:14, 4:6) = 1;
B3 = imgaussfilt3(V, 1.0, 'FilterSize', [5 5 5], 'Padding', 0);
asserts.isequal(size(B3), size(V));
asserts.istrue(B3(11, 11, 5) > B3(1, 1, 1));
[X, Y, Z] = meshgrid(linspace(-1, 1, 12), linspace(-1, 1, 10), linspace(-1, 1, 6));
V = exp(-6 * (X .^ 2 + Y .^ 2 + Z .^ 2));
B3R = imresize3(V, [16 16 8], 'linear');
asserts.isequal(size(B3R), [16 16 8]);
asserts.istrue(B3R(8, 8, 4) > B3R(1, 1, 1));
[X, Y, Z] = meshgrid(1:24, 1:24, 1:10);
BW3 = ((X - 8) .^ 2 + (Y - 9) .^ 2 + (Z - 4) .^ 2) < 4 ^ 2;
BW3 = BW3 | (((X - 17) .^ 2 + (Y - 16) .^ 2 + (Z - 7) .^ 2) < 3 ^ 2);
S3 = regionprops3(BW3, 'Volume', 'Centroid', 'BoundingBox');
asserts.istrue(length(S3) >= 2);
asserts.istrue(isfield(S3, 'Volume'));
%=============================================================================
asserts.checkerror('imboxfilt([1 2], 0)', sprintf(_('%s size must contain positive integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('imboxfilt([1 2], [1.5 3])', sprintf(_('%s size must contain positive integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('imboxfilt([1 2], [2 + i 3])', sprintf(_('%s size must contain positive integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('imboxfilt([1 2], 3, ''NormalizationFactor'', Inf)', sprintf(_('%s must be a finite numeric scalar.'), 'Normalization factor'));
%=============================================================================
asserts.checkerror('imboxfilt([1 2], ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'imboxfilt'));
