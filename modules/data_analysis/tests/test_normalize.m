%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('normalize'), -2);
asserts.isequal(nargout('normalize'), 3);
%=============================================================================
% default: z-score (center mean, scale std)
asserts.isapprox(normalize([1 2 3 4 5]), [-1.264911064067352 -0.632455532033676 0 0.632455532033676 1.264911064067352], 1e-12);
%=============================================================================
asserts.isequal(normalize([2 4 6], 'range'), [0 0.5 1]);
asserts.isequal(normalize([1 2 3], 'range', [0 10]), [0 5 10]);
asserts.isapprox(norm(normalize([3 4], 'norm')), 1, 1e-12);
asserts.isequal(normalize([1 2 3], 'center'), [-1 0 1]);
asserts.isequal(normalize([1 2 3], 'center', 'median'), [-1 0 1]);
%=============================================================================
% scale by std
asserts.isapprox(normalize([2 4 6 8], 'scale'), [2 4 6 8] / std([2 4 6 8]), 1e-12);
%=============================================================================
% [centered, C, S] outputs
[B, C, S] = normalize([1 2 3 4 5]);
asserts.isequal(C, 3);
asserts.isapprox(S, std([1 2 3 4 5]), 1e-12);
asserts.isapprox(B, ([1 2 3 4 5] - C) / S, 1e-12);
%=============================================================================
% works column-wise on a matrix by default
M = [1 10; 2 20; 3 30];
asserts.isapprox(normalize(M), [normalize([1 2 3]') normalize([10 20 30]')], 1e-12);
%=============================================================================
asserts.checkerror('normalize([1 2 3], ''unknownmethod'')', sprintf(_('Unsupported "%s" option.'), 'unknownmethod'), 'Nelson:normalize:invalidMethod');
%=============================================================================
