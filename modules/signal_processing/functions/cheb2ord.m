%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [n, wn] = cheb2ord(varargin)
  narginchk(4, 4);
  nargoutchk(0, 2);
  wp = varargin{1};
  ws = varargin{2};
  rp = varargin{3};
  rs = varargin{4};
  passband = tan(pi * wp / 2);
  stopband = tan(pi * ws / 2);
  passbandFactor = sqrt(10 ^ (rp / 10) - 1);
  stopbandFactor = sqrt(10 ^ (rs / 10) - 1);
  if length(wp) == 1 && length(ws) == 1
    if ws > wp
      ratio = stopband / passband;
    else
      ratio = passband / stopband;
    end
    wn = ws;
  elseif length(wp) == 2 && length(ws) == 2
    if ws(1) < wp(1) && wp(2) < ws(2)
      bandwidth = passband(2) - passband(1);
      center = sqrt(passband(1) * passband(2));
      values = abs((stopband .^ 2 - center ^ 2) ./ (bandwidth * stopband));
    elseif wp(1) < ws(1) && ws(2) < wp(2)
      bandwidth = stopband(2) - stopband(1);
      center = sqrt(stopband(1) * stopband(2));
      values = abs((passband .^ 2 - center ^ 2) ./ (bandwidth * passband));
    else
      error('Nelson:signal:inconsistentFrequencies', 'Passband and stopband frequencies are inconsistent.');
    end
    ratio = min(values);
    wn = ws;
  else
    error('Nelson:dimagree:valueLength', '%s must have the same length.', 'Passband and stopband frequencies');
  end
  n = ceil(acosh(stopbandFactor / passbandFactor) / acosh(ratio));
end
%=============================================================================
