%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The scalar integer exponent fast path (a.^2, a.^3, a.^4) is bit-for-bit equal
% to the repeated multiplication it replaces (same operation order).
rng(42);
r = rand(200, 200) * 4 + 0.01;
asserts.isequal(r .^ 2, r .* r);
asserts.isequal(r .^ 3, r .* r .* r);
asserts.isequal(r .^ 4, (r .* r) .* (r .* r));
asserts.isequal(r .^ (-2), 1 ./ (r .* r));
asserts.isequal(r .^ (-3), 1 ./ (r .* r .* r));
%=============================================================================
% negative bases keep the exact integer-power sign
b = -r;
asserts.isequal(b .^ 2, r .* r);
asserts.isequal(b .^ 3, -(r .* r .* r));
asserts.isequal(b .^ 4, (r .* r) .* (r .* r));
%=============================================================================
% exact-integer bases: values are independent of the multiplication grouping,
% so they pin down the whole intercepted exponent range 2..64
base = reshape(-8:7, 4, 4);
for e = [2 3 4 5 6 7 8 10 16 32]
  asserts.isequal(base .^ e, arrayfun(@(x) x ^ e, base));
end
asserts.isequal((2 * ones(3)) .^ 64, 2 ^ 64 * ones(3));
%=============================================================================
% scalar base, small integer exponents, exact values
asserts.isequal(2 .^ 2, 4);
asserts.isequal(3 .^ 3, 27);
asserts.isequal(2 .^ 10, 1024);
asserts.isequal(10 .^ 4, 10000);
asserts.isequal((-2) .^ 2, 4);
asserts.isequal((-2) .^ 3, -8);
asserts.isequal((-2) .^ 4, 16);
asserts.isequal((-2) .^ (-2), 0.25);
asserts.isequal((-2) .^ (-3), -0.125);
%=============================================================================
% IEEE special values preserved by the fast path
asserts.isequal(Inf .^ 2, Inf);
asserts.isequal((-Inf) .^ 2, Inf);
asserts.isequal((-Inf) .^ 3, -Inf);
asserts.isequal(0 .^ 2, 0);
asserts.isequal(0 .^ 3, 0);
asserts.isequal(1 ./ ((-0) .^ 3), -Inf);
asserts.isequal((-0) .^ 2, 0);
asserts.isequal(1 ./ ((-0) .^ 2), Inf);
asserts.isequal((-0) .^ (-2), Inf);
asserts.isequal(realmax .^ 2, Inf);
R = NaN .^ 2;
assert(isnan(R));
R = NaN .^ 3;
assert(isnan(R));
%=============================================================================
% vector base / vector exponent still correct (non-scalar-exponent paths)
asserts.isequal([2 3 4 5] .^ 2, [4 9 16 25]);
asserts.isequal(2 .^ [2 3 4 5], [4 8 16 32]);
%=============================================================================
% single precision benefits from the same fast path and stays single
rs = single(r);
asserts.isequal(rs .^ 2, rs .* rs);
asserts.isequal(class(rs .^ 2), 'single');
asserts.isequal(single(3) .^ 4, single(81));
asserts.isequal(single(-2) .^ 3, single(-8));
asserts.isequal(single(2) .^ 10, single(1024));
%=============================================================================
% exponents outside the intercepted range still compute correctly
asserts.isequal(2 .^ 65, 2 ^ 65);
asserts.isequal(r .^ 1, r);
asserts.isequal(r .^ 0, ones(size(r)));
%=============================================================================
