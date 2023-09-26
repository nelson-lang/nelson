%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = zp2tf(varargin)
  narginchk(3, 3);
  nargoutchk(0, 2);
  z = varargin{1};
  p = varargin{2};
  k = varargin{3};
  
  den = computesDenominator(p);
  varargout{1} = computesNumerator(z, k, den);
  
  if nargout > 1
    varargout{2} = den;
  end
end
%=============================================================================
function den = computesDenominator(p)
  den = real(poly(p(:)));
end
%=============================================================================
function num = computesNumerator(z, k, den)
  num = [];
  denc = size(den, 2);
  k = k(:);
  kr = size(k, 1);
  if isempty(z)
    if (denc - 1 > 0)
      num = [zeros(kr, denc - 1), k];
    else
      num = k;
    end
  else
    zc = size(z, 2);
    if kr ~= zc
      if ((size(z, 1) - 1) ~= 0)
        error('Input argument #1 and #3 should have the same column size.');
      else
        error('First argument must be a column vector.');
      end
    else
      num = [];
      for nidx = 1:zc
        zidx = z(:, nidx);
        pidx = real(poly(zidx) * k(nidx));
        zr = zeros(1, denc - length(pidx));
        if isempty(num)
          num = [zr, pidx];
        else
          num(nidx, :) = [zr, pidx];
        end
      end
    end
  end
end
%=============================================================================
