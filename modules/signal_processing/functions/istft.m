%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function x = istft(varargin)
  narginchk(1, 10);
  nargoutchk(0, 1);
  s = varargin{1};
  window = hamming(128);
  overlapLength = 96;
  fftLength = (size(s, 1) - 1) * 2;
  frequencyRange = 'centered';
  if nargin >= 2 && isnumeric(varargin{2})
    k = 3;
  else
    k = 2;
  end
  while k <= nargin
    if k == nargin
      error('Nelson:signal:nameValueNotPaired', 'Name-value options must be paired.');
    end
    if ~(ischar(varargin{k}) || isStringScalar(varargin{k}))
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Option name');
    end
    name = char(varargin{k});
    value = varargin{k + 1};
    if strcmpi(name, 'Window')
      window = value(:);
    elseif strcmpi(name, 'OverlapLength')
      overlapLength = value;
    elseif strcmpi(name, 'FFTLength')
      fftLength = value;
    elseif strcmpi(name, 'FrequencyRange')
      frequencyRange = value;
    else
      error('Nelson:signal:unsupportedOption', 'Unsupported option.');
    end
    k = k + 2;
  end
  validateIstftParameters(window, overlapLength, fftLength);
  frequencyRange = validateIstftFrequencyRange(frequencyRange);
  if size(s, 1) == floor(fftLength / 2) + 1
    s = [s; conj(s(end - 1:-1:2, :))];
  elseif size(s, 1) == fftLength && strcmpi(frequencyRange, 'centered')
    negativeCount = floor((fftLength - 1) / 2);
    s = [s(negativeCount + 1:end, :); s(1:negativeCount, :)];
  end
  step = length(window) - overlapLength;
  outputLength = step * (size(s, 2) - 1) + length(window);
  x = zeros(outputLength, 1);
  weight = zeros(outputLength, 1);
  for idx = 1:size(s, 2)
    frame = real(ifft(s(:, idx), fftLength));
    frame = frame(1:length(window)) .* window;
    start = (idx - 1) * step + 1;
    range = start:start + length(window) - 1;
    x(range) = x(range) + frame;
    weight(range) = weight(range) + window .^ 2;
  end
  valid = weight > eps();
  x(valid) = x(valid) ./ weight(valid);
end
%=============================================================================
function frequencyRange = validateIstftFrequencyRange(frequencyRange)
  if ~(ischar(frequencyRange) || isStringScalar(frequencyRange))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Frequency range');
  end
  frequencyRange = lower(convertStringsToChars(frequencyRange));
  if ~(strcmp(frequencyRange, 'centered') || strcmp(frequencyRange, 'onesided') ...
      || strcmp(frequencyRange, 'twosided'))
    error('Nelson:signal:unsupportedFrequencyRange', 'Unsupported frequency range.');
  end
end
%=============================================================================
function validateIstftParameters(window, overlapLength, fftLength)
  if overlapLength >= length(window)
    error('Nelson:signal:invalidOverlapLength', 'Overlap length must be less than window length.');
  end
  if fftLength < length(window)
    error('Nelson:signal:invalidFFTLength', 'FFT length must be greater than or equal to window length.');
  end
end
%=============================================================================
