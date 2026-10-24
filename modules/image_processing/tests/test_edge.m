%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[X, Y] = meshgrid(linspace(-1, 1, 24), linspace(-1, 1, 16));
I = exp(-4 * (X .^ 2 + Y .^ 2));
BW = edge(I, 'sobel');
asserts.istrue(islogical(BW));
asserts.istrue(any(BW(:)));
[level, effectiveness] = graythresh(I);
asserts.istrue(level >= 0 && level <= 1);
asserts.istrue(effectiveness >= 0 && effectiveness <= 1);
%=============================================================================
asserts.checkerror('edge([1 2], 1)', sprintf(_('%s must be a string.'), 'Edge detection method'));
%=============================================================================
asserts.checkerror('edge([1 2], ''sobel'', Inf)', sprintf(_('%s must be a finite numeric scalar.'), 'Threshold'));
