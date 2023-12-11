%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = compreal(varargin)
  narginchk(2, 2);
  nargoutchk(0, 5);
  
  numerator = varargin{1};
  denominator = varargin{2};
  
  E = [];
  kE = [];
  [p, r] = size(numerator);
  if (r == 1) || (norm(numerator, 1) == 0)
    [A, B, C, D] = handlePureGainOrZeroNumerator(numerator, denominator, p);
  else
    [A, B, C, D, E, kE] = buildCompanionRealization(numerator, denominator, r);
  end
  [A, B, C, E] = balanceCompanionForm(A, B, C, E, kE);
  
  varargout{1} = A;
  if nargout > 1
    varargout{2} = B;
  end
  if nargout > 2 
    varargout{3} = C;
  end
  if nargout > 3
    varargout{4} = D;
  end
  if nargout > 4
    varargout{5} = E;
  end
end
%=============================================================================
function [A, B, C, D, E, kE] = buildCompanionRealization(numerator, denominator, r)
  idx = find(denominator ~= 0, 1);
  denominator1 = denominator(idx);
  denominator = denominator(idx + 1:r) / denominator1;
  numerator = numerator / denominator1;
  
  ld = length(denominator);
  if idx ~=1
    [A, B, C, D, E, kE] = buildImproperCaseCompanionRealization(numerator, denominator, idx);
  else
    [A, B, C, D, E, kE] = buildProperCaseCompanionRealization(numerator, denominator, r);
  end
end
%=============================================================================
function [A, B, C, D, E, kE] = buildProperCaseCompanionRealization(numerator, denominator, r)
  ld = length(denominator);
  A = [-denominator ; eye(ld - 1, ld)];
  B = eye(ld, 1);
  D = numerator(:, 1);
  C = numerator(:, 2:r) - D * denominator;
  E = [];
  kE = 0;
end
%=============================================================================
function [A, B, C, D, E, kE] = buildImproperCaseCompanionRealization(numerator, denominator, idx)
  ld = length(denominator);
  ld1 = min(1, ld);
  A = blkdiag(eye(idx), diag(ones(ld1, ld - 1), -1));
  A(idx + ld1, idx + 1:idx + ld) = -denominator;
  B = zeros(idx + ld, 1 );
  if ld <= 0
    B(idx) = -1;
  else
    B(idx + 1) = 1;
  end
  C = numerator;
  D = zeros(p, 1);
  E = diag([zeros(idx, 1); ones(ld, 1)]);
  stride = idx + ld + 1;
  E(stride:stride:(idx - 1 + ld1) * stride) = 1;
  kE = idx;
end
%=============================================================================
function [A, B, C, D] = handlePureGainOrZeroNumerator(numerator, denominator, p)
  A = [];
  B = zeros(0, 1);
  C = zeros(p, 0);
  D = numerator(:, 1) / denominator(:, 1);
end
%=============================================================================
function [A, B, C, E] = balanceCompanionForm(A, B, C, E, kE)
  nx = size(A, 1);
  
  if nx - kE < 2
    rs = zeros(0, 1);
  else
    [dummy, dummy, x] = balance(A(kE+1:nx,kE+1:nx), 'noperm');
    rs = diag(x, -1);
  end
  
  idx = find(rs(2:end) < min(1, 1e-3 * rs(1:end - 1)));
  if ~isempty(idx)
    rsmin = min(1, rs(idx(1)));
    rs = max(rs, rsmin);
  end
  
  if (kE > 0)
    if (kE < nx)
      kE = kE + 1;
    end
    vc = max(abs(C(:, 1:kE)), [], 1);
    vc = vc/vc(1);
    [dummy, dummy, x] = balance([vc; eye(kE - 1, kE)], 'noperm');
    rs = [diag(x, -1); rs];
  end
  
  s = cumprod([1; rs]);
  A = lrscale(A, s, 1 ./ s);
  B = lrscale(B, s, []);
  C = lrscale(C, [], 1 ./ s);
  if ~isempty(E)
    E = lrscale(E, s, 1 ./ s);
  end
  
  cnorm = norm(C, 1);
  bnorm = norm(B, 1);
  if cnorm > 0
    sbc = pow2(round(log2(cnorm/bnorm)/2)); 
    C = C / sbc;
    B = B * sbc;
  end
end
%=============================================================================
function sx = lrscale(X, L, R)
  L = lscale(X, L);
  R = rscale(X, R);
  sx = performScaling(X, L, R);
end
%=============================================================================
function L = lscale(X, L)
  cL = size(L, 2);
  if cL == 0
    rX = size(X, 1);
    if rX > 0
      L = ones(rX, 1);
    else
      L = [];
    end
  else
    if cL > 1
      L = L.';
    end
  end
end
%=============================================================================
function R = rscale(X, R)
  rR = size(R, 1);
  if rR == 0
    cX = size(X, 2);
    if cX > 0
      R = ones(1, cX);
    else
      R = [];
    end
  else
    if rR > 1
      R = R.';
    end
  end
end
%=============================================================================
function scaledMatrix = performScaling(X, L, R)
  scaledMatrix = X .* (L * R);
end
%=============================================================================
