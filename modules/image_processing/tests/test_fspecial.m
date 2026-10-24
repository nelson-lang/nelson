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
I = double([1 2 3; 4 5 6; 7 8 9]);
H = fspecial('average', [3 3]);
J = imfilter(I, H, 'replicate');
REF = [7 9 11; 13 15 17; 19 21 23] / 3;
asserts.isapprox(J, REF, 1e-14);
%=============================================================================
H = fspecial('sobel');
REF_SOBEL = zeros(3, 3);
REF_SOBEL(1, :) = [1 2 1];
REF_SOBEL(3, :) = -REF_SOBEL(1, :);
asserts.isequal(H, REF_SOBEL);
REF_PREWITT = zeros(3, 3);
REF_PREWITT(1, :) = [1 1 1];
REF_PREWITT(3, :) = -REF_PREWITT(1, :);
asserts.isequal(fspecial('prewitt'), REF_PREWITT);
A = [1 2; 3 4];
F = imfilter(A, [1 0; 0 0], 'full');
asserts.isequal(size(F), [3 3]);
V = imfilter(A, [1 0; 0 0], 'valid');
asserts.isequal(size(V), [1 1]);
%=============================================================================
asserts.checkerror('fspecial(''unknown'')', sprintf(_('Unsupported "%s" option.'), 'filter type'));
%=============================================================================
asserts.checkerror('fspecial(''average'', [2 + i 2])', sprintf(_('%s size must contain positive integers.'), 'Filter'));
%=============================================================================
asserts.checkerror('fspecial(''gaussian'', [3 3], 0)', sprintf(_('%s must be a positive finite scalar.'), 'Sigma'));
%=============================================================================
asserts.checkerror('fspecial(''disk'', -1)', sprintf(_('%s must be a nonnegative finite scalar.'), 'Disk radius'));
%=============================================================================
asserts.checkerror('fspecial(''laplacian'', 2)', sprintf(_('%s must be a finite scalar in the range [%s].'), 'Alpha', '0, 1'));
