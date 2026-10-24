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
% The internal predicate: element-wise broadcasting bodies are accepted.
asserts.istrue(__is_elementwise_broadcastable__(@(x, y) x + y));
asserts.istrue(__is_elementwise_broadcastable__(@(x, y) sqrt(x .^ 2 + y .^ 2)));
asserts.istrue(__is_elementwise_broadcastable__(@(x, y) x .* y ./ (1 + y)));
asserts.istrue(__is_elementwise_broadcastable__(@(x, y) power(x, y)));
asserts.istrue(__is_elementwise_broadcastable__(@(x, y) atan2(x, y)));
asserts.istrue(__is_elementwise_broadcastable__(@(x, y) x > y));
% Rejected: functions that do NOT broadcast in Nelson.
asserts.isfalse(__is_elementwise_broadcastable__(@(x, y) min(x, y)));
asserts.isfalse(__is_elementwise_broadcastable__(@(x, y) max(x, y)));
asserts.isfalse(__is_elementwise_broadcastable__(@(x, y) mod(x, y)));
asserts.isfalse(__is_elementwise_broadcastable__(@(x, y) rem(x, y)));
% Rejected: matrix operators, indexing.
asserts.isfalse(__is_elementwise_broadcastable__(@(x, y) x * y));
asserts.isfalse(__is_elementwise_broadcastable__(@(x, y) x / y));
asserts.isfalse(__is_elementwise_broadcastable__(@(x) x(1)));
%=============================================================================
% Named handles: the standard binary element-wise builtins that broadcast are
% accepted (fast path); min/max/mod/rem (which do not broadcast) are rejected.
named_ok = {@plus, @minus, @times, @rdivide, @ldivide, @power, @hypot, @atan2, ...
  @lt, @le, @gt, @ge, @eq, @ne, @and, @or};
for kk = 1:numel(named_ok)
  asserts.istrue(__is_elementwise_broadcastable__(named_ok{kk}));
end
asserts.isfalse(__is_elementwise_broadcastable__(@max));
asserts.isfalse(__is_elementwise_broadcastable__(@min));
asserts.isfalse(__is_elementwise_broadcastable__(@mod));
asserts.isfalse(__is_elementwise_broadcastable__(@rem));
%=============================================================================
