%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% resample filters only the samples kept after decimation. The result must
% stay the one of the plain definition (interpolate, filter the whole signal,
% keep every q-th sample from the filter delay), whatever the residue of that
% delay modulo q, and non-finite samples must spread exactly as far.
t = 1:200;
x = sin(0.1 * t) + 0.3 * cos(0.37 * t);
xz = x;
xz(50) = Inf;
xz(120) = NaN;
X = [x; cos(0.2 * t)];
b = ones(1, 8) / 8;
CASES = {
  x, 36, 25, [], 2
  x, 7, 3, [], 2
  x(:), 7, 3, [], 1
  x, 5, 3, [], 2
  x, 25, 36, [], 2
  x, 2, 3, [], 2
  x, 3, 1, [], 2
  x, 1, 4, [], 2
  xz, 7, 3, [], 2
  xz, 36, 25, [], 2
  X, 7, 3, b, 2
  X.', 7, 3, b, 1
  X, 11, 4, fir1(12, 0.2), 2
  single(x), 7, 3, [], 2
  int16(1:50), 7, 3, [], 2
  x(1:3), 7, 3, [], 2
  x(1:5), 36, 25, ones(1, 4), 2
};
for k = 1:size(CASES, 1)
  [xk, p, q, h, dim] = CASES{k, :};
  if isempty(h)
    L = 20 * max(p, q);
    h = p * fir1(L, 1 / max(p, q), kaiser(L + 1, 5));
    y = resample(xk, p, q, 'Dimension', dim);
  else
    y = resample(xk, p, q, h, 'Dimension', dim);
  end
  d = ceil((length(h) - 1) / 2);
  full = upfirdn(xk, h, p, 1, dim);
  n = ceil(size(xk, dim) * p / q);
  if dim == 1
    ref = full(d + 1:q:end, :);
    ref = ref(1:min(n, size(ref, 1)), :);
  else
    ref = full(:, d + 1:q:end);
    ref = ref(:, 1:min(n, size(ref, 2)));
  end
  asserts.istrue(isequaln(y, ref));
end
%=============================================================================
% 30 minutes at 360 Hz brought to 250 Hz: only the 450000 kept samples go
% through the 721-tap filter, well under a second (the full convolution of the
% 16 million sample interpolated signal took 25 s).
x = randn(1, 648000);
tic();
y = resample(x, 25, 36);
elapsed = toc();
asserts.isequal(size(y), [1 450000]);
asserts.istrue(elapsed < 3);
%=============================================================================
