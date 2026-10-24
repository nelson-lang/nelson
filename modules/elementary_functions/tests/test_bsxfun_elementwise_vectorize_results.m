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
% Auto-vectorization fast path of bsxfun: when the anonymous callback body is a
% provably element-wise AND broadcasting-safe expression, bsxfun evaluates it
% once on the original operands (Nelson broadcasts singleton dimensions) instead
% of materializing both operands with repmat. Results must be identical to the
% repmat-based path in every case, and any body that is not provably broadcast
% safe must fall back to that path unchanged.
%=============================================================================
A = (1:3)'; % 3 x 1
B = (10:10:40); % 1 x 4  -> broadcast to 3 x 4
Afull = repmat(A, 1, 4);
Bfull = repmat(B, 3, 1);
%=============================================================================
% Fast-path results equal the repmat-based reference.
asserts.isequal(bsxfun(@(x, y) x + y, A, B), Afull + Bfull);
asserts.isequal(bsxfun(@(x, y) x - y, A, B), Afull - Bfull);
asserts.isequal(bsxfun(@(x, y) x .* y, A, B), Afull .* Bfull);
asserts.isequal(bsxfun(@(x, y) sqrt(x .^ 2 + y .^ 2), A, B), sqrt(Afull .^ 2 + Bfull .^ 2));
asserts.isequal(bsxfun(@(x, y) x .^ y, A, B), Afull .^ Bfull);
asserts.isequal(bsxfun(@(x, y) x > y, A, B), Afull > Bfull);
asserts.isequal(bsxfun(@atan2, A, B), atan2(Afull, Bfull));
asserts.isequal(bsxfun(@(x, y) hypot(x, y), A, B), hypot(Afull, Bfull));
%=============================================================================
% Named-handle fast path: results equal the repmat-based reference.
asserts.isequal(bsxfun(@plus, A, B), Afull + Bfull);
asserts.isequal(bsxfun(@minus, A, B), Afull - Bfull);
asserts.isequal(bsxfun(@times, A, B), Afull .* Bfull);
asserts.isequal(bsxfun(@rdivide, A, B), Afull ./ Bfull);
asserts.isequal(bsxfun(@ldivide, A, B), Bfull ./ Afull);
asserts.isequal(bsxfun(@power, A, B), Afull .^ Bfull);
asserts.isequal(bsxfun(@hypot, A, B), hypot(Afull, Bfull));
asserts.isequal(bsxfun(@lt, A, B), Afull < Bfull);
asserts.isequal(bsxfun(@ge, A, B), Afull >= Bfull);
asserts.isequal(bsxfun(@and, A, B), Afull & Bfull);
asserts.isequal(bsxfun(@or, A, B), Afull | Bfull);
asserts.isequal(bsxfun('plus', A, B), Afull + Bfull);
% Fall-back bodies (min/max/mod/rem and their named handles do not broadcast)
% must still be correct and equal to the repmat-based reference.
asserts.isequal(bsxfun(@(x, y) min(x, y), A, B), min(Afull, Bfull));
asserts.isequal(bsxfun(@(x, y) max(x, y), A, B), max(Afull, Bfull));
asserts.isequal(bsxfun(@(x, y) mod(x, y), A, B), mod(Afull, Bfull));
asserts.isequal(bsxfun(@max, A, B), max(Afull, Bfull));
asserts.isequal(bsxfun(@mod, A, B), mod(Afull, Bfull));
%=============================================================================
% Captured scalar in the body stays correct on the fast path.
k = 3.5;
asserts.isequal(bsxfun(@(x, y) x + y + k, A, B), Afull + Bfull + k);
%=============================================================================
% Scalar and same-size operands.
asserts.isequal(bsxfun(@(x, y) x + y, A, 100), A + 100);
M = magic(4);
asserts.isequal(bsxfun(@(x, y) x .* y, M, M), M .* M);
%=============================================================================
% Broadcasting on both operands.
C = (1:3)';
D = 1:4;
asserts.isequal(bsxfun(@(x, y) x + y, C, D), C + D);
%=============================================================================
% Error on genuinely incompatible dimensions is preserved.
asserts.checkerror('bsxfun(@(x, y) x + y, ones(2, 3), ones(4, 5))', ...
  _('Non-singleton dimensions of the two input arrays must match each other.'), ...
  'Nelson:bsxfun:arrayDimensionsMustMatch');
%=============================================================================
