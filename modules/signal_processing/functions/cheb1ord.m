%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [n, wn] = cheb1ord(varargin)
  narginchk(4, 4);
  nargoutchk(0, 2);
  wp = varargin{1};
  ws = varargin{2};
  rp = varargin{3};
  rs = varargin{4};
  if length(wp) == 2 && length(ws) == 2
    passband = tan(pi * wp(:).' / 2);
    stopband = tan(pi * ws(:).' / 2);
    if ws(1) < wp(1) && wp(2) < ws(2)
      bandwidth = passband(2) - passband(1);
      center2 = passband(1) * passband(2);
      ratio = min(abs((stopband .^ 2 - center2) ./ (bandwidth * stopband)));
    elseif wp(1) < ws(1) && ws(2) < wp(2)
      bandwidth = stopband(2) - stopband(1);
      center2 = stopband(1) * stopband(2);
      ratio = min(abs((passband .^ 2 - center2) ./ (bandwidth * passband)));
    else
      error('Nelson:signal:inconsistentFrequencies', 'Passband and stopband frequencies are inconsistent.');
    end
    epsilon = sqrt(10 ^ (rp / 10) - 1);
    stopbandFactor = sqrt(10 ^ (rs / 10) - 1);
    n = ceil(acosh(stopbandFactor / epsilon) / acosh(ratio));
    wn = wp;
    return;
  end
  if length(wp) ~= 1 || length(ws) ~= 1
    error('Nelson:dimagree:valueLength', '%s must have the same length.', 'Passband and stopband frequencies');
  end
  passband = tan(pi * wp / 2);
  stopband = tan(pi * ws / 2);
  if ws > wp
    ratio = stopband / passband;
  else
    ratio = passband / stopband;
  end
  epsilon = sqrt(10 ^ (rp / 10) - 1);
  stopbandFactor = sqrt(10 ^ (rs / 10) - 1);
  n = ceil(acosh(stopbandFactor / epsilon) / acosh(ratio));
  wn = wp;
end
%=============================================================================
