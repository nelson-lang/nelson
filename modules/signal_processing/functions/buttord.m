%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [n, wn] = buttord(wp, ws, rp, rs)
  narginchk(4, 4);
  nargoutchk(0, 2);
  passbandFactor = 10 ^ (rp / 10) - 1;
  stopbandFactor = 10 ^ (rs / 10) - 1;
  if length(wp) == 2 && length(ws) == 2
    passband = tan(pi * wp(:).' / 2);
    stopband = tan(pi * ws(:).' / 2);
    if ws(1) < wp(1) && wp(2) < ws(2)
      bandwidth = passband(2) - passband(1);
      center2 = passband(1) * passband(2);
      ratio = min(abs((stopband .^ 2 - center2) ./ (bandwidth * stopband)));
      n = ceil(log10(stopbandFactor / passbandFactor) / (2 * log10(ratio)));
      adjustedBandwidth = bandwidth * ratio / (stopbandFactor ^ (1 / (2 * n)));
      cutoffSum = sqrt(adjustedBandwidth ^ 2 + 4 * center2);
      cutoff = sort(roots([1, -cutoffSum, center2])).';
    elseif wp(1) < ws(1) && ws(2) < wp(2)
      bandwidth = stopband(2) - stopband(1);
      center2 = stopband(1) * stopband(2);
      ratio = min(abs((passband .^ 2 - center2) ./ (bandwidth * passband)));
      n = ceil(log10(stopbandFactor / passbandFactor) / (2 * log10(ratio)));
      adjustedBandwidth = bandwidth * (stopbandFactor ^ (1 / (2 * n)));
      cutoffSum = sqrt(adjustedBandwidth ^ 2 + 4 * center2);
      cutoff = sort(roots([1, -cutoffSum, center2])).';
    else
      error('Nelson:signal:inconsistentFrequencies', 'Passband and stopband frequencies are inconsistent.');
    end
    wn = (2 / pi) * atan(cutoff);
    wn = min(max(real(wn), eps()), 1 - eps());
    return;
  end
  if length(wp) ~= 1 || length(ws) ~= 1
    error('Nelson:dimagree:valueLength', '%s must have the same length.', 'Passband and stopband frequencies');
  end
  passband = tan(pi * wp / 2);
  stopband = tan(pi * ws / 2);
  if ws > wp
    ratio = stopband / passband;
    n = ceil(log10(stopbandFactor / passbandFactor) / (2 * log10(ratio)));
    cutoff = stopband / (stopbandFactor ^ (1 / (2 * n)));
  else
    ratio = passband / stopband;
    n = ceil(log10(stopbandFactor / passbandFactor) / (2 * log10(ratio)));
    cutoff = stopband * (stopbandFactor ^ (1 / (2 * n)));
  end
  wn = (2 / pi) * atan(cutoff);
  wn = min(max(wn, eps()), 1 - eps());
end
%=============================================================================
