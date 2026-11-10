%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = safeNewton(residual, slope, t, lowLimit, highLimit)
  % Newton iterations on an increasing residual, kept inside a bracket that bisection narrows
  lo = lowLimit + zeros(size(t), 'like', t);
  hi = highLimit + zeros(size(t), 'like', t);
  tol = 4 * eps(underlyingType(t));
  active = true(size(t));
  for iteration = 1:200
    idx = find(active);
    if isempty(idx)
      break
    end
    ta = t(idx);
    r = residual(ta, idx);
    above = r > 0;
    hiA = hi(idx);
    loA = lo(idx);
    hiA(above) = ta(above);
    loA(~above) = ta(~above);
    step = r ./ slope(ta, idx);
    next = ta - step;
    outside = ~(next >= loA & next <= hiA);
    next(outside) = (loA(outside) + hiA(outside)) / 2;
    next(r == 0) = ta(r == 0);
    done = r == 0 | abs(next - ta) <= tol * max(1, abs(ta)) | hiA - loA <= tol * max(1, abs(ta));
    t(idx) = next;
    lo(idx) = loA;
    hi(idx) = hiA;
    active(idx(done)) = false;
  end
end
%=============================================================================
