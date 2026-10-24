%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stft(varargin)
  narginchk(1, 10);
  nargoutchk(0, 3);
  x = varargin{1}(:);
  fs = 1;
  window = hamming(128);
  overlapLength = 96;
  fftLength = 128;
  frequencyRange = 'centered';
  if nargin >= 2 && isnumeric(varargin{2})
    fs = varargin{2};
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
  validateStftParameters(window, overlapLength, fftLength);
  frequencyRange = validateStftFrequencyRange(frequencyRange);
  [s, f, t] = signalStftCore(x, window, overlapLength, fftLength, fs, frequencyRange);
  if nargout == 0
    imagesc(t, f, mag2db(abs(s)));
  else
    varargout{1} = s;
    if nargout > 1
      varargout{2} = f;
    end
    if nargout > 2
      varargout{3} = t;
    end
  end
end
%=============================================================================
function frequencyRange = validateStftFrequencyRange(frequencyRange)
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
function validateStftParameters(window, overlapLength, fftLength)
  if overlapLength >= length(window)
    error('Nelson:signal:invalidOverlapLength', 'Overlap length must be less than window length.');
  end
  if fftLength < length(window)
    error('Nelson:signal:invalidFFTLength', 'FFT length must be greater than or equal to window length.');
  end
end
%=============================================================================
