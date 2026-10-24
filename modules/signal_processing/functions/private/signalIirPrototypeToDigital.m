%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [digitalZeros, digitalPoles, gain, b, a] = signalIirPrototypeToDigital(z, p, n, rp, wn, ftype)
  if mod(n, 2) == 0
    targetGain = 10 ^ (-rp / 20);
  else
    targetGain = 1;
  end
  if length(wn) == 1
    [az, ap, appendInf, normalizeAt] = signalIirScalarTransform(z, p, n, wn, ftype);
  else
    [az, ap, appendInf, normalizeAt] = signalIirBandTransform(z, p, n, wn, ftype);
  end
  digitalZeros = (1 + az / 2) ./ (1 - az / 2);
  digitalPoles = (1 + ap / 2) ./ (1 - ap / 2);
  if appendInf > 0
    digitalZeros = [digitalZeros; -ones(appendInf, 1)];
  end
  b = real(poly(digitalZeros));
  a = real(poly(digitalPoles));
  gain = targetGain * abs(polyval(a, normalizeAt) / polyval(b, normalizeAt));
  b = b * gain;
end
%=============================================================================
function [az, ap, appendInf, normalizeAt] = signalIirScalarTransform(z, p, n, wn, ftype)
  if ~(isempty(ftype) || strcmpi(ftype, 'low') || strcmpi(ftype, 'high'))
    error('Nelson:signal:cutoffScalarLowpassHighpass', 'Cutoff frequency must be a scalar for lowpass and highpass filters.');
  end
  warped = 2 * tan(pi * wn / 2);
  if strcmpi(ftype, 'high')
    az = warped ./ z(:);
    az = [az; zeros(n - length(z), 1)];
    ap = warped ./ p(:);
    appendInf = 0;
    normalizeAt = -1;
  else
    az = warped * z(:);
    ap = warped * p(:);
    appendInf = n - length(z);
    normalizeAt = 1;
  end
end
%=============================================================================
function [az, ap, appendInf, normalizeAt] = signalIirBandTransform(z, p, n, wn, ftype)
  if ~(isempty(ftype) || strcmpi(ftype, 'bandpass') || strcmpi(ftype, 'pass') || strcmpi(ftype, 'stop'))
    error('Nelson:signal:cutoffVectorBandpassBandstop', 'Cutoff frequency must be a two-element vector for bandpass and bandstop filters.');
  end
  warped = 2 * tan(pi * wn(:).' / 2);
  bandwidth = warped(2) - warped(1);
  center = sqrt(warped(1) * warped(2));
  ap = signalMapBandPoles(p, bandwidth, center, strcmpi(ftype, 'stop'));
  if strcmpi(ftype, 'stop')
    az = signalMapBandPoles(z, bandwidth, center, true);
    missing = n - length(z);
    az = [az; 1i * center * ones(missing, 1); -1i * center * ones(missing, 1)];
    appendInf = 0;
    normalizeAt = 1;
  else
    az = signalMapBandPoles(z, bandwidth, center, false);
    az = [az; zeros(n - length(z), 1)];
    appendInf = n - length(z);
    normalizeAt = exp(1i * 2 * atan(center / 2));
  end
end
%=============================================================================
function mapped = signalMapBandPoles(values, bandwidth, center, isStop)
  mapped = [];
  for idx = 1:length(values)
    if isStop
      mapped = [mapped; roots([values(idx), -bandwidth, values(idx) * center ^ 2])];
    else
      mapped = [mapped; roots([1, -bandwidth * values(idx), center ^ 2])];
    end
  end
end
%=============================================================================
