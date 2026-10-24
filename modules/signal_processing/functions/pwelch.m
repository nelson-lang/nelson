%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pwelch(varargin)
  narginchk(1, 7);
  nargoutchk(0, 2);
  x = varargin{1};
  win = [];
  overlap = [];
  nfft = [];
  fs = 1;
  range = '';
  spectrumType = 'psd';
  optionStart = 6;
  if nargin >= 2
    win = varargin{2};
  end
  if nargin >= 3
    overlap = varargin{3};
  end
  if nargin >= 4
    nfft = varargin{4};
  end
  if nargin >= 5
    if ischar(varargin{5}) || isStringScalar(varargin{5})
      optionStart = 5;
    else
      fs = varargin{5};
    end
  end
  for k = optionStart:nargin
    option = lower(char(varargin{k}));
    if strcmp(option, 'onesided') || strcmp(option, 'twosided') || strcmp(option, 'centered') ...
        || strcmp(option, 'half') || strcmp(option, 'whole')
      range = option;
    elseif strcmp(option, 'psd') || strcmp(option, 'power')
      spectrumType = option;
    else
      error('Nelson:signal:unsupportedOption', 'Unsupported option.');
    end
  end
  [pxx, f] = signalWelchCore(x, [], win, overlap, nfft, fs, range, spectrumType);
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
