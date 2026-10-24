%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
I3 = zeros(2, 2, 3);
I3(:, :, 1) = [1 2; 3 4];
I3(:, :, 2) = [5 6; 7 8];
I3(:, :, 3) = [9 10; 11 12];
asserts.isequal(size(imcrop(I3, [1 1 0 0])), [1 1 3]);
A = [1 2; 3 4];
asserts.isequal(imwarp(A, eye(3), 'Interpolation', 'nearest'), A);
asserts.checkerror('edge(A, ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'edge detection method'));
%=============================================================================
asserts.checkerror('imcrop([1 2], [1 2 3])', sprintf(_('%s must be a numeric 4-element vector.'), 'Crop rectangle'));
%=============================================================================
asserts.checkerror('imcrop([1 2], [1 1 NaN 1])', sprintf(_('%s must contain finite values.'), 'Crop rectangle'));
%=============================================================================
asserts.checkerror('imcrop([1 2], [1 1 -1 1])', sprintf(_('%s width and height must be nonnegative.'), 'Crop rectangle'));
%=============================================================================
I = reshape(1:9, 3, 3);
C = imcrop(I, [1 1 1 1]);
asserts.isequal(C, [1 4; 2 5]);
%=============================================================================
