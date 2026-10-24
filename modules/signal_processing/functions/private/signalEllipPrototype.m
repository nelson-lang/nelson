%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [z, p] = signalEllipPrototype(n, rp, rs)
  if n == 1
    z = [];
    p = -sqrt(1 / (10 ^ (rp / 10) - 1));
    return;
  end
  epsilon2 = 10 ^ (rp / 10) - 1;
  ck12 = epsilon2 / (10 ^ (rs / 10) - 1);
  m = signalEllipDegree(n, ck12);
  capk = signalEllipke(m);
  j = 1 - mod(n, 2):2:n - 1;
  [s, c, d] = signalEllipj(j * capk / n, m);
  snew = s(abs(s) > eps());
  z = 1i ./ (sqrt(m) * snew(:));
  z = [z; conj(z)];
  r = signalEllipArcSc1(1 / sqrt(epsilon2), ck12);
  v0 = capk * r / (n * signalEllipke(ck12));
  [sv, cv, dv] = signalEllipj(v0, 1 - m);
  p = -(c(:) .* d(:) * sv * cv + 1i * s(:) * dv) ./ (1 - (d(:) * sv) .^ 2);
  if mod(n, 2) == 1
    keep = abs(imag(p)) > eps() * sqrt(sum(real(p .* conj(p))));
    p = [p; conj(p(keep))];
  else
    p = [p; conj(p)];
  end
end
%=============================================================================
