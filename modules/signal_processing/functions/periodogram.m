%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = periodogram(varargin)
  narginchk(1, 6);
  nargoutchk(0, 2);
  x = varargin{1};
  win = [];
  nfft = [];
  fs = 1;
  range = '';
  spectrumType = 'psd';
  if nargin >= 2
    win = varargin{2};
  end
  if nargin >= 3
    nfft = varargin{3};
  end
  if nargin >= 4
    fs = varargin{4};
  end
  for k = 5:nargin
    option = lower(char(varargin{k}));
    if strcmp(option, 'half')
      range = 'onesided';
    elseif strcmp(option, 'whole')
      range = 'twosided';
    elseif strcmp(option, 'onesided') || strcmp(option, 'twosided') || strcmp(option, 'centered')
      range = option;
    elseif strcmp(option, 'psd') || strcmp(option, 'power')
      spectrumType = option;
    else
      error('Nelson:signal:unsupportedOption', 'Unsupported option.');
    end
  end
  [pxx, f] = signalPeriodogramCore(x, win, nfft, fs, range, spectrumType);
  if nargout == 0
    plot(f, pow2db(pxx));
  else
    varargout{1} = pxx;
    if nargout > 1
      varargout{2} = f;
    end
  end
end
%=============================================================================
