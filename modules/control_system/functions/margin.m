%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = margin(varargin)
  % Gain and phase margins of a SISO LTI system.
  %
  % margin(sys)
  % [Gm, Pm, Wcg, Wcp] = margin(sys)
  % [Gm, Pm, Wcg, Wcp] = margin(mag, phase, w)
  %
  % Gm  : gain margin (absolute, 1/|G(j*Wcg)|)
  % Pm  : phase margin (degrees)
  % Wcg : phase crossover frequency (rad/s), where phase = -180 deg
  % Wcp : gain crossover frequency (rad/s), where |G| = 1 (0 dB)

  narginchk(1, 3);
  nargoutchk(0, 4);

  if (nargin == 1)
    [Gm, Pm, Wcg, Wcp] = marginFromSystem(varargin{1});
  elseif (nargin == 3)
    [Gm, Pm, Wcg, Wcp] = marginFromData(varargin{1}, varargin{2}, varargin{3});
  else
    error('Nelson:control_system:wrongNumberOfArguments', 'Wrong number of input arguments.');
  end

  if (nargout == 0)
    if (nargin == 1)
      bode(varargin{1});
    else
      error('Nelson:control_system:invalidOutputArgument', 'margin with frequency-response data requires output arguments.');
    end
    return
  end

  varargout{1} = Gm;
  if (nargout > 1)
    varargout{2} = Pm;
  end
  if (nargout > 2)
    varargout{3} = Wcg;
  end
  if (nargout > 3)
    varargout{4} = Wcp;
  end
end
%=============================================================================
function [Gm, Pm, Wcg, Wcp] = marginFromSystem(sys)
  if ~islti(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end
  if ~issiso(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end
  sys = tf(sys);
  a = sys.Numerator{1};
  b = sys.Denominator{1};
  relativeDegree = polyDegree(b) - polyDegree(a);
  n = max(length(a), length(b));
  a = [zeros(1, n - length(a)) a];
  b = [zeros(1, n - length(b)) b];
  Ts = sys.Ts;

  w = frequencyGrid(a, b, Ts);
  H = evalResponse(a, b, Ts, w);
  mag = abs(H);
  phaseDeg = unwrapPhase(angle(H)) * 180 / pi;

  % Gain crossover (|H| = 1) -> phase margin.
  [Wcp, Pm] = gainCrossover(a, b, Ts, w, mag, phaseDeg);
  % Phase crossover (H on negative real axis) -> gain margin.
  [Wcg, Gm] = phaseCrossover(a, b, Ts, w, H);
  % A continuous system of relative degree >= 2 whose phase only reaches
  % -180 degrees at infinity has an infinite gain margin located at w = Inf.
  if (isnan(Wcg) && (Ts <= 0) && (relativeDegree >= 2))
    Wcg = Inf;
    Gm = Inf;
  end
end
%=============================================================================
function [Wcp, Pm] = gainCrossover(a, b, Ts, w, mag, phaseDeg)
  logw = log10(w);
  d = mag - 1;
  idx = find(d(1:end - 1) .* d(2:end) < 0);
  Wcp = NaN;
  Pm = Inf;
  for k = idx(:).'
    lo = logw(k);
    hi = logw(k + 1);
    for it = 1:80
      mid = 0.5 * (lo + hi);
      fm = abs(evalResponse(a, b, Ts, 10 ^ mid)) - 1;
      if ((abs(evalResponse(a, b, Ts, 10 ^ lo)) - 1) * fm <= 0)
        hi = mid;
      else
        lo = mid;
      end
    end
    wc = 10 ^ (0.5 * (lo + hi));
    phc = linearInterp(logw, phaseDeg, log10(wc));
    candidate = 180 + phc;
    if (candidate < Pm)
      Pm = candidate;
      Wcp = wc;
    end
  end
end
%=============================================================================
function [Wcg, Gm] = phaseCrossover(a, b, Ts, w, H)
  logw = log10(w);
  re = real(H);
  im = imag(H);
  idx = find((im(1:end - 1) .* im(2:end) < 0) & ((re(1:end - 1) < 0) | (re(2:end) < 0)));
  Wcg = NaN;
  Gm = Inf;
  for k = idx(:).'
    lo = logw(k);
    hi = logw(k + 1);
    imlo = imag(evalResponse(a, b, Ts, 10 ^ lo));
    for it = 1:80
      mid = 0.5 * (lo + hi);
      fm = imag(evalResponse(a, b, Ts, 10 ^ mid));
      if (imlo * fm <= 0)
        hi = mid;
      else
        lo = mid;
        imlo = fm;
      end
    end
    wc = 10 ^ (0.5 * (lo + hi));
    candidate = 1 / abs(evalResponse(a, b, Ts, wc));
    if (candidate < Gm)
      Gm = candidate;
      Wcg = wc;
    end
  end
end
%=============================================================================
function [Gm, Pm, Wcg, Wcp] = marginFromData(mag, phaseDeg, w)
  if (~isnumeric(mag) || ~isnumeric(phaseDeg) || ~isnumeric(w))
    error('Nelson:control_system:invalidType', '%s expected.', 'numeric');
  end
  mag = real(mag(:)).';
  phaseDeg = real(phaseDeg(:)).';
  w = real(w(:)).';
  if (numel(mag) ~= numel(w) || numel(phaseDeg) ~= numel(w))
    error('Nelson:control_system:invalidSize', '%s', 'mag, phase and w must have the same number of elements.');
  end
  [w, order] = sort(w);
  mag = mag(order);
  phaseDeg = phaseDeg(order);
  logw = log10(w);

  % Gain crossover (|G| = 1) -> phase margin.
  d = mag - 1;
  idx = find(d(1:end - 1) .* d(2:end) < 0);
  Wcp = NaN;
  Pm = Inf;
  for k = idx(:).'
    wc = crossFrequency(logw(k), logw(k + 1), mag(k) - 1, mag(k + 1) - 1);
    candidate = 180 + linearInterp(logw, phaseDeg, wc);
    if (candidate < Pm)
      Pm = candidate;
      Wcp = 10 ^ wc;
    end
  end

  % Phase crossover (negative real axis) -> gain margin.
  s = sin(phaseDeg * pi / 180);
  c = cos(phaseDeg * pi / 180);
  idx = find((s(1:end - 1) .* s(2:end) < 0) & ((c(1:end - 1) < 0) | (c(2:end) < 0)));
  Wcg = NaN;
  Gm = Inf;
  for k = idx(:).'
    wc = crossFrequency(logw(k), logw(k + 1), s(k), s(k + 1));
    candidate = 1 / (10 ^ linearInterp(logw, log10(mag), wc));
    if (candidate < Gm)
      Gm = candidate;
      Wcg = 10 ^ wc;
    end
  end
end
%=============================================================================
function x = crossFrequency(x0, x1, f0, f1)
  if (f1 == f0)
    x = x0;
  else
    x = x0 - f0 * (x1 - x0) / (f1 - f0);
  end
end
%=============================================================================
function y = linearInterp(x, v, xq)
  if (xq <= x(1))
    y = v(1);
    return
  end
  if (xq >= x(end))
    y = v(end);
    return
  end
  k = find(x <= xq, 1, 'last');
  if (x(k + 1) == x(k))
    y = v(k);
  else
    t = (xq - x(k)) / (x(k + 1) - x(k));
    y = v(k) + t * (v(k + 1) - v(k));
  end
end
%=============================================================================
function d = polyDegree(p)
  nz = find(abs(p) > 0, 1, 'first');
  if isempty(nz)
    d = 0;
  else
    d = length(p) - nz;
  end
end
%=============================================================================
function H = evalResponse(a, b, Ts, w)
  if (Ts > 0)
    z = exp(1i * w * Ts);
  else
    z = 1i * w;
  end
  H = polyval(a, z) ./ polyval(b, z);
end
%=============================================================================
function w = frequencyGrid(a, b, Ts)
  L = 4000;
  zp = [roots(a(:)); roots(b(:))];
  if (Ts > 0)
    nyq = pi / Ts;
    zp = zp(abs(zp) > sqrt(eps) & abs(zp - 1) > sqrt(eps));
    if isempty(zp)
      wmin = log10(nyq) - 4;
    else
      mags = abs(log(zp) / Ts);
      mags = mags(mags > 0 & isfinite(mags));
      if isempty(mags)
        wmin = log10(nyq) - 4;
      else
        wmin = floor(log10(min(mags))) - 2;
      end
    end
    wmax = log10(nyq);
  else
    zp = zp(abs(zp) > sqrt(eps));
    if isempty(zp)
      wmin = -3;
      wmax = 3;
    else
      wmin = floor(log10(min(abs(zp)))) - 3;
      wmax = ceil(log10(max(abs(zp)))) + 3;
    end
  end
  if (wmax <= wmin)
    wmax = wmin + 6;
  end
  w = logspace(wmin, wmax, L);
end
%=============================================================================
function u = unwrapPhase(p)
  if (length(p) > 1)
    twopi = 2 * pi;
    p(2:end) = p(2:end) - cumsum(twopi * round(diff(p) / twopi));
  end
  u = p;
end
%=============================================================================
