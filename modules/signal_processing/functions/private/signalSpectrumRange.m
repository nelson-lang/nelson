%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [spectrum, f] = signalSpectrumRange(rawSpectrum, nfft, fs, range, useRealSymmetry)
  if strcmp(range, 'half')
    range = 'onesided';
  elseif strcmp(range, 'whole')
    range = 'twosided';
  end
  if isempty(range)
    if useRealSymmetry
      range = 'onesided';
    else
      range = 'twosided';
    end
  end
  if strcmp(range, 'onesided') && useRealSymmetry
    [spectrum, f] = localOnesided(rawSpectrum, nfft, fs);
  elseif strcmp(range, 'centered') && useRealSymmetry
    [spectrum, f] = localCenteredReal(rawSpectrum, nfft, fs);
  else
    [spectrum, f] = localTwosided(rawSpectrum, nfft, fs, range);
  end
end
%=============================================================================
function [spectrum, f] = localOnesided(rawSpectrum, nfft, fs)
  if mod(nfft, 2) == 0
    select = 1:nfft / 2 + 1;
    spectrum = rawSpectrum(select);
    spectrum(2:end - 1) = 2 * spectrum(2:end - 1);
  else
    select = 1:(nfft + 1) / 2;
    spectrum = rawSpectrum(select);
    spectrum(2:end) = 2 * spectrum(2:end);
  end
  f = (select(:) - 1) * fs / nfft;
end
%=============================================================================
function [spectrum, f] = localCenteredReal(rawSpectrum, nfft, fs)
  if mod(nfft, 2) == 0
    positive = rawSpectrum(1:nfft / 2 + 1);
    spectrum = [positive(nfft / 2:-1:2); positive(1); positive(2:nfft / 2); positive(end)];
    f = ([-(nfft / 2 - 1):-1, 0:nfft / 2].') * fs / nfft;
  else
    positive = rawSpectrum(1:(nfft + 1) / 2);
    spectrum = [positive(end:-1:2); positive];
    f = ([-floor(nfft / 2):-1, 0:floor(nfft / 2)].') * fs / nfft;
  end
end
%=============================================================================
function [spectrum, f] = localTwosided(rawSpectrum, nfft, fs, range)
  if strcmp(range, 'centered')
    spectrum = fftshift(rawSpectrum);
    if mod(nfft, 2) == 0
      f = (-nfft / 2:nfft / 2 - 1)' * fs / nfft;
    else
      f = (-(nfft - 1) / 2:(nfft - 1) / 2)' * fs / nfft;
    end
  else
    spectrum = rawSpectrum;
    f = (0:nfft - 1)' * fs / nfft;
  end
end
%=============================================================================
