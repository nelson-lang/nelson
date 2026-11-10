%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Vr, Dr] = cdf2rdf(V, D)
  % turns complex conjugate eigenvalue pairs into real 2-by-2 blocks with matching real vectors
  narginchk(2, 2);
  nargoutchk(0, 2);
  n = size(D, 1);
  if ~ismatrix(D) || size(D, 2) ~= n || size(V, 2) ~= n
    error('Nelson:cdf2rdf:invalidInput', 'D must be square and V must have as many columns as D.');
  end
  d = diag(D);
  Vr = V;
  Dr = D;
  k = 1;
  while k <= n
    if imag(d(k)) == 0
      k = k + 1;
      continue
    end
    if k == n || d(k + 1) ~= conj(d(k))
      error('Nelson:cdf2rdf:notConjugatePairs', 'The diagonal of D must hold real values and complex conjugate pairs.');
    end
    a = real(d(k));
    b = imag(d(k));
    Dr(k:k + 1, k:k + 1) = [a, b; -b, a];
    Vr(:, k:k + 1) = sqrt(2) * [real(V(:, k)), imag(V(:, k))];
    k = k + 2;
  end
  Vr = real(Vr);
  Dr = real(Dr);
end
%=============================================================================
