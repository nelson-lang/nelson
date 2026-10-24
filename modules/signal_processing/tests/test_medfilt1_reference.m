%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% medfilt1 agrees with median applied to every zero-padded or truncated window
n = 400;
x = sin((1:n) * 0.37) + cos((1:n) * 1.3);
x([5 6 120 399]) = NaN;
x(250) = Inf;
for order = [4 5]
  before = floor(order / 2);
  after = order - before - 1;
  padded = [zeros(1, before), x, zeros(1, after)];
  zeropad = zeros(1, n);
  zeropadOmit = zeros(1, n);
  truncate = zeros(1, n);
  truncateOmit = zeros(1, n);
  for k = 1:n
    w = padded(k:k + order - 1);
    zeropad(k) = median(w);
    zeropadOmit(k) = median(w, 'omitnan');
    w = x(max(1, k - before):min(n, k + after));
    truncate(k) = median(w);
    truncateOmit(k) = median(w, 'omitnan');
  end
  asserts.isequal(medfilt1(x, order), zeropad);
  asserts.isequal(medfilt1(x, order, 'includenan'), zeropad);
  asserts.isequal(medfilt1(x, order, 'omitnan'), zeropadOmit);
  asserts.isequal(medfilt1(x, order, 'truncate'), truncate);
  asserts.isequal(medfilt1(x, order, 'omitnan', 'truncate'), truncateOmit);
  asserts.isequal(medfilt1(x', order), zeropad');
  asserts.isequal(medfilt1([x; 2 * x], order, [], 2), [zeropad; 2 * zeropad]);
end
%=============================================================================
% single precision and integer values
xs = single(x);
ref = zeros(1, n);
for k = 1:n
  ref(k) = median(xs(max(1, k - 1):min(n, k + 1)), 'omitnan');
end
asserts.isequal(medfilt1(xs, 3, 'omitnan', 'truncate'), ref);
x8 = int16(round(1000 * (sin((1:n) * 0.37) + cos((1:n) * 1.3))));
padded = [0, 0, x8, 0];
ref = zeros(1, n);
for k = 1:n
  ref(k) = double(median(padded(k:k + 3)));
end
asserts.isequal(medfilt1(x8, 4), ref);
%=============================================================================
