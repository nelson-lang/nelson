%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tfe(u, y, nfft, fs)
  arguments
    u {mustBeNumeric, mustBeVector}
    y {mustBeNumeric, mustBeVector}
    nfft {mustBeNumeric} = []
    fs {mustBeNumeric} = []
  end
  % Transfer function estimate compatibility wrapper.
  % [Hv, f, Puu, Pyy, Puy, coh, Sv] = tfe(u, y, nfft, fs)

  nargoutchk(0, 7);

  u = u(:);
  y = y(:);
  if length(u) ~= length(y)
    error('Nelson:dimagree:valueLength', '%s must have the same length.', 'Input signals');
  end

  n = length(u);
  if isempty(nfft)
    nfft = min(256, n);
  end
  if isempty(fs)
    fs = 2;
  end

  window = localHanning(nfft);
  noverlap = 0;
  nwind = length(window);
  if n < nwind
    u(nwind) = 0;
    y(nwind) = 0;
    n = nwind;
  end

  nwindows = fix((n - noverlap) / (nwind - noverlap));

  Puu = zeros(nfft, 1);
  Pyy = zeros(nfft, 1);
  Puy = zeros(nfft, 1);
  idx = 1:nwind;
  for w = 1:nwindows
    uw = window .* u(idx);
    yw = window .* y(idx);
    idx = idx + (nwind - noverlap);
    U = fft(uw, nfft);
    Y = fft(yw, nfft);
    Puu = Puu + abs(U) .^ 2;
    Pyy = Pyy + abs(Y) .^ 2;
    Puy = Puy + Y .* conj(U);
  end

  if isreal(u) && isreal(y)
    if rem(nfft, 2)
      select = (1:(nfft + 1) / 2)';
    else
      select = (1:nfft / 2 + 1)';
    end
    Puu = Puu(select);
    Pyy = Pyy(select);
    Puy = Puy(select);
  else
    select = (1:nfft)';
  end

  Hv = Puy ./ Puu;
  f = (select - 1) * fs / nfft;

  if nargout == 0
    plot(f, abs(Hv));
    return
  end

  varargout{1} = Hv;
  if nargout > 1
    varargout{2} = f;
  end
  if nargout > 2
    varargout{3} = Puu;
  end
  if nargout > 3
    varargout{4} = Pyy;
  end
  if nargout > 4
    varargout{5} = Puy;
  end
  if nargout > 5
    varargout{6} = (abs(Puy) .^ 2) ./ (Puu .* Pyy);
  end
  if nargout > 6
    varargout{7} = abs(Hv) ./ sqrt(max(1, nwindows));
  end
end
%=============================================================================
function window = localHanning(nfft)
  window = 0.5 * (1 - cos(2 * pi * (1:nfft)' / (nfft + 1)));
end
%=============================================================================
