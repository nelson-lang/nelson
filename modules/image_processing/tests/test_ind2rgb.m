%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
X = repmat(uint8(0:15), 16, 1);
v = linspace(0, 1, 16)';
map = [v 1 - v 0.5 * ones(16, 1)];
RGB = ind2rgb(X, map);
G = ind2gray(X, map);
asserts.isequal(size(RGB), [16 16 3]);
asserts.isequal(size(G), [16 16]);
asserts.isapprox(G, rgb2gray(RGB), 1e-15);
map = [1 0 0; 0 1 0; 0 0 1];
RGB = ind2rgb([0 1 2 5], map);
asserts.isequal(reshape(RGB, [4 3]), [1 0 0; 1 0 0; 0 1 0; 0 0 1]);
%=============================================================================
asserts.checkerror('ind2rgb([1.5 2], [1 0 0; 0 1 0])', _('Indexed image values must be finite integers.'));
%=============================================================================
asserts.checkerror('ind2rgb(uint8([0 1]), [1 0; 0 1])', _('Colormap must have at least three columns.'));
