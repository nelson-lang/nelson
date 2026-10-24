%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[H, S] = meshgrid(linspace(0, 1, 24), linspace(0, 1, 16));
HSV = zeros(16, 24, 3);
HSV(:, :, 1) = H;
HSV(:, :, 2) = S;
HSV(:, :, 3) = 1;
RGB = hsv2rgb(HSV);
asserts.isequal(size(RGB), [16 24 3]);
asserts.isapprox(reshape(RGB(1, 1, :), [1 3]), [1 1 1], 1e-15);
asserts.isapprox(reshape(RGB(16, 24, :), [1 3]), [1 0 0], 1e-15);
%=============================================================================
asserts.checkerror('hsv2rgb(ones(2, 2))', sprintf(_('%s must be an M-by-N-by-3 array.'), 'HSV image'));
