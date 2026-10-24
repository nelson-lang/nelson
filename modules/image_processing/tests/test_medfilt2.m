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
I = peaks(24);
I(6:4:18, 6:4:18) = 8;
J = medfilt2(I, [3 3]);
asserts.isequal(size(J), size(I));
asserts.istrue(max(J(:)) < 8);
I = eye(16);
P = padarray(I, [4 6], 'replicate');
asserts.isequal(size(P), [24 28]);
asserts.isequal([P(1, 1), P(5, 7), P(end, end)], [1 1 1]);
I = zeros(32, 32);
I(10:22, 10:22) = 1;
C = cornermetric(I);
PH = detectHarrisFeatures(I, 'MinQuality', 0.05);
PF = detectFASTFeatures(I, 'MinContrast', 0.2);
asserts.isequal(size(C), size(I));
asserts.isequal(PH.Count, 4);
asserts.istrue(PF.Count >= 4);
%=============================================================================
asserts.checkerror('medfilt2([1 2], [3 0])', sprintf(_('%s size must contain positive integers.'), 'Neighborhood'));
%=============================================================================
asserts.checkerror('medfilt2([1 2], [3 3], ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'padding'));
%=============================================================================
asserts.checkerror('medfilt2(zeros(2, 2, 3))', sprintf(_('%s must be a 2-D array.'), 'Image'));
