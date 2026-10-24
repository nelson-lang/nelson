%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% movmedian and movmad agree with median applied to every window,
% NaN and Inf values included.
n = 300;
x = sin((1:n) * 0.37) + cos((1:n) * 1.3);
x([5 6 120 299]) = NaN;
x(200) = Inf;
x(201) = -Inf;
ref = zeros(1, n);
refOmit = zeros(1, n);
mad = zeros(1, n);
madOmit = zeros(1, n);
for k = 1:n
  w = x(max(1, k - 2):min(n, k + 2));
  ref(k) = median(w);
  refOmit(k) = median(w, 'omitnan');
  mad(k) = median(abs(w - median(w)));
  v = w(~isnan(w));
  if isempty(v)
    madOmit(k) = NaN;
  else
    madOmit(k) = median(abs(v - median(v)));
  end
end
asserts.isequal(movmedian(x, 5), ref);
asserts.isequal(movmedian(x, 5, 'includenan'), ref);
asserts.isequal(movmedian(x, 5, 'omitnan'), refOmit);
asserts.isequal(movmedian(x, 5, 'Endpoints', 'discard'), ref(3:end - 2));
asserts.isequal(movmedian(x, 5, 'Endpoints', 'fill'), [NaN NaN ref(3:end - 2) NaN NaN]);
asserts.isequal(movmedian(x, 5, 'omitnan', 'Endpoints', 'fill'), refOmit);
asserts.isequal(movmad(x, 5), mad);
asserts.isequal(movmad(x, 5, 'omitnan'), madOmit);
asserts.isequal(movmad(x, 5, 'Endpoints', 'discard'), mad(3:end - 2));
%=============================================================================
% asymmetric window along the second dimension of a matrix
X = [x; x(end:-1:1)];
ref = zeros(2, n);
for k = 1:n
  ref(:, k) = median(X(:, max(1, k - 3):min(n, k + 1)), 2);
end
asserts.isequal(movmedian(X, [3 1], 2), ref);
%=============================================================================
% single precision values
xs = single(x);
ref = zeros(1, n);
for k = 1:n
  ref(k) = median(xs(max(1, k - 1):min(n, k + 2)));
end
asserts.isequal(movmedian(xs, [1 2]), ref);
%=============================================================================
% integer values: the median of two values is rounded, deviations saturate
x8 = int8(round(60 * (sin((1:n) * 0.37) + cos((1:n) * 1.3))));
ref = zeros(1, n);
mad = zeros(1, n);
for k = 1:n
  w = x8(max(1, k - 2):min(n, k + 1));
  ref(k) = double(median(w));
  mad(k) = double(median(abs(w - median(w))));
end
asserts.isequal(movmedian(x8, 4), ref);
asserts.isequal(movmad(x8, 4), mad);
%=============================================================================
