%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = snr(varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  x = varargin{1};
  if nargin == 2 && ~isscalar(varargin{2})
    noise = varargin{2};
    if ~isequal(size(x), size(noise))
      error('Nelson:dimagree:signalNoiseDimensions', 'Signal and noise must have the same dimensions.');
    end
    signalPower = norm(x(:)) ^ 2;
    noisePower = norm(noise(:)) ^ 2;
    outputNoisePower = noisePower;
  else
    fs = 1;
    nHarmonics = 6;
    if nargin >= 2
      fs = varargin{2};
    end
    if nargin == 3
      nHarmonics = varargin{3};
    end
    [signalPower, noisePower] = spectralSignalNoisePower(x, fs, nHarmonics);
    outputNoisePower = 10 * log10(noisePower);
  end
  r = 10 * log10(signalPower / noisePower);
  varargout{1} = r;
  if nargout > 1
    varargout{2} = outputNoisePower;
  end
end
%=============================================================================
function [signalPower, noisePower] = spectralSignalNoisePower(x, fs, nHarmonics)
  if ~isscalar(fs) || fs <= 0
    error('Nelson:signal:snr:invalidSampleRate', '%s must be a positive scalar.', 'Sample rate');
  end
  if ~isscalar(nHarmonics) || nHarmonics < 1
    error('Nelson:signal:snr:invalidHarmonics', '%s must be a positive scalar.', 'Number of harmonics');
  end
  x = x(:) - mean(x(:));
  n = length(x);
  spectrum = abs(fft(x) / n) .^ 2;
  keep = 1:floor(n / 2) + 1;
  spectrum = spectrum(keep);
  if length(spectrum) > 2
    spectrum(2:end - 1) = 2 * spectrum(2:end - 1);
  end
  spectrum(1) = 0;
  [signalPower, fundamentalIndex] = max(spectrum);
  harmonicIndexes = fundamentalIndex:fundamentalIndex:length(spectrum);
  harmonicIndexes = harmonicIndexes(1:min(length(harmonicIndexes), nHarmonics));
  noiseSpectrum = spectrum;
  noiseSpectrum(harmonicIndexes) = 0;
  noisePower = sum(noiseSpectrum);
  if noisePower == 0
    noisePower = eps(signalPower);
  end
end
%=============================================================================
