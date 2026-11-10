%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = expintCore(X)
  % E1 with a power series near the origin and on the left half plane, a continued fraction elsewhere
  isComplex = ~isreal(X);
  Y = zeros(size(X), 'like', X);
  z = reshape(X, [], 1);
  y = zeros(size(z), 'like', X);
  tol = eps(underlyingType(X));
  series = abs(z) <= 2 | (real(z) < 0 & abs(imag(z)) <= max(2, -real(z)));
  special = ~isfinite(z) | z == 0;
  series = series & ~special;
  fraction = ~series & ~special;
  if any(series)
    y(series) = powerSeries(z(series), tol);
  end
  if any(fraction)
    y(fraction) = continuedFraction(z(fraction), tol, realmin(underlyingType(X)));
  end
  % zero gives Inf, infinite values give NaN, and minus infinity keeps the branch cut part
  y(z == 0) = Inf;
  if ~isComplex
    y(~isfinite(z)) = NaN;
  else
    y(~isfinite(z)) = complex(NaN, NaN);
    y(~isfinite(z) & imag(z) == 0) = NaN;
    y(real(z) == -Inf & imag(z) == 0) = complex(NaN, -pi);
  end
  Y(:) = y;
end
%=============================================================================
function y = powerSeries(s, tol)
  % -gamma - log(s) - sum((-s)^k / (k k!)); converged entries leave the working set
  y = zeros(size(s), 'like', s);
  pos = (1:numel(s)).';
  term = ones(size(s), 'like', s);
  total = zeros(size(s), 'like', s);
  for k = 1:5000
    term = term .* (-s) / k;
    total = total + term / k;
    done = abs(term) <= tol * abs(total) * k;
    if any(done)
      y(pos(done)) = -0.577215664901532860606512090082 - log(s(done)) - total(done);
      keep = ~done;
      pos = pos(keep);
      s = s(keep);
      term = term(keep);
      total = total(keep);
      if isempty(pos)
        break
      end
    end
  end
end
%=============================================================================
function y = continuedFraction(s, tol, tiny)
  % modified Lentz evaluation of exp(-s) / (s + 1 - 1 / (s + 3 - 4 / (s + 5 - ...)))
  y = zeros(size(s), 'like', s);
  pos = (1:numel(s)).';
  b = s + 1;
  c = ones(size(s), 'like', s) / tiny;
  d = 1 ./ b;
  h = d;
  for k = 1:10000
    an = -k * k;
    b = b + 2;
    d = 1 ./ (an * d + b);
    c = b + an ./ c;
    delta = c .* d;
    h = h .* delta;
    done = abs(delta - 1) <= tol;
    if any(done)
      y(pos(done)) = h(done) .* exp(-s(done));
      keep = ~done;
      pos = pos(keep);
      s = s(keep);
      b = b(keep);
      c = c(keep);
      d = d(keep);
      h = h(keep);
      if isempty(pos)
        break
      end
    end
  end
end
%=============================================================================
