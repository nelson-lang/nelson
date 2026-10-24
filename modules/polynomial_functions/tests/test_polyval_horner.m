%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The fused C++ Horner builtin must be bit-identical to the elementwise
% multiply-then-add loop it replaces.
%=============================================================================
% Known values.
asserts.isequal(polyval_horner([3 2 1], [5 7 9]), [86 162 262]);
asserts.isequal(polyval_horner([1 0 -1], 2), 3);
%=============================================================================
% Degree 0 (single coefficient) broadcasts over x.
asserts.isequal(polyval_horner(7, [1 2; 3 4]), 7 + zeros(2, 2));
asserts.isequal(polyval_horner(-2.5, zeros(0, 3)), zeros(0, 3));
%=============================================================================
% Bit-identity with the reference elementwise loop, real double.
degrees = {0, 1, 2, 6};
for k = 1:numel(degrees)
  p = rand(1, degrees{k} + 1) - 0.5;
  x = (rand(40, 7) - 0.5) * 10;
  asserts.isequal(polyval_horner(p, x), refHorner(p, x));
end
%=============================================================================
% Scalar, row, column, N-D shapes are preserved.
p = [2 -3 0.5 4];
asserts.isequal(polyval_horner(p, 1.25), refHorner(p, 1.25));
xnd = reshape((1:24) / 7, [2 3 4]);
r = polyval_horner(p, xnd);
asserts.isequal(size(r), [2 3 4]);
asserts.isequal(r, refHorner(p, xnd));
%=============================================================================
% NaN / Inf in the evaluation points and in the coefficients propagate
% exactly like the loop.
x = [NaN, Inf, -Inf, 0, 1, -2];
asserts.isequal(polyval_horner([2 1 3], x), refHorner([2 1 3], x));
pbad = [1 NaN 2];
asserts.isequal(polyval_horner(pbad, [1 2 3]), refHorner(pbad, [1 2 3]));
pinf = [Inf 1 1];
asserts.isequal(polyval_horner(pinf, [0 2]), refHorner(pinf, [0 2]));
%=============================================================================
% single keeps single precision and is bit-identical.
ps = single([1.5 -2.25 0.75]);
xs = single((rand(50, 1) - 0.5) * 6);
rs = polyval_horner(ps, xs);
asserts.isequal(class(rs), 'single');
asserts.isequal(rs, refHorner(ps, xs));
% Mixed single/double promotes to single.
asserts.isequal(class(polyval_horner(single([1 2 3]), 2)), 'single');
asserts.isequal(class(polyval_horner([1 2 3], single(2))), 'single');
asserts.isequal(polyval_horner([1 2 3], single(2)), refHorner([1 2 3], single(2)));
%=============================================================================
% Complex coefficients and/or complex evaluation points are bit-identical to
% the elementwise loop, including the leading/trailing SIMD-packet boundaries.
pc = [1 0 -1];
zc = [1+1i, 2-3i, -1i, 0.5+0.5i];
rc = polyval_horner(pc, zc);
asserts.isfalse(isreal(rc));
asserts.isequal(rc, refHorner(pc, zc));
pc2 = [2+1i, -1+0i, 3-2i, 0.5];
asserts.isequal(polyval_horner(pc2, zc), refHorner(pc2, zc));
% Complex coefficients with real points.
asserts.isequal(polyval_horner([1+2i, 3-1i, 0.5], [0 1 2 3]), ...
  refHorner([1+2i, 3-1i, 0.5], [0 1 2 3]));
% single complex.
pcs = single([1+1i, -2, 0.5-1i]);
zcs = single((rand(97, 1) - 0.5) + 1i * (rand(97, 1) - 0.5));
rcs = polyval_horner(pcs, zcs);
asserts.isequal(class(rcs), 'single');
asserts.isequal(rcs, refHorner(pcs, zcs));
% Bit-identity across element counts (N=1 scalar path, odd/even packet tails,
% and a large array) for double and single.
pz = [2+1i, -1+0.5i, 3-2i, 0.5, 1-1i, 0.25, -0.5+0.5i, 2];
for N = [1 2 3 5 8 17 101 4001]
  z = (rand(N, 1) - 0.5) + 1i * (rand(N, 1) - 0.5);
  asserts.isequal(polyval_horner(pz, z), refHorner(pz, z));
  asserts.isequal(polyval_horner(single(pz), single(z)), refHorner(single(pz), single(z)));
end
%=============================================================================
% A complex computation whose result is purely real collapses to real.
asserts.istrue(isreal(polyval_horner([1 0 1], [2 3 4] + 0i)));
%=============================================================================
% The public polyval routes through the builtin and stays correct.
asserts.isequal(polyval([3 2 1], [5 7 9]), [86 162 262]);
p = rand(1, 7) - 0.5;
x = rand(100, 1);
asserts.isequal(polyval(p, x), refHorner(p, x));
pcbig = [2+1i, -1+0i, 3-2i, 0.5];
zbig = (rand(5000, 1) - 0.5) + 1i * (rand(5000, 1) - 0.5);
asserts.isequal(polyval(pcbig, zbig), refHorner(pcbig, zbig));
%=============================================================================
function y = refHorner(coefficients, evaluationPoints)
  % Exact reproduction of the historical elementwise multiply-then-add loop.
  y = coefficients(1) + zeros(size(evaluationPoints));
  for coefficientIndex = 2:numel(coefficients)
    y = y .* evaluationPoints + coefficients(coefficientIndex);
  end
end
%=============================================================================
