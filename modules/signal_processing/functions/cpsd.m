%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cpsd(varargin)
  narginchk(2, 7);
  nargoutchk(0, 2);
  x = varargin{1};
  y = varargin{2};
  win = [];
  overlap = [];
  nfft = [];
  fs = 1;
  range = '';
  optionStart = 7;
  if nargin >= 3
    win = varargin{3};
  end
  if nargin >= 4
    overlap = varargin{4};
  end
  if nargin >= 5
    nfft = varargin{5};
  end
  if nargin >= 6
    if ischar(varargin{6}) || isStringScalar(varargin{6})
      optionStart = 6;
    else
      fs = varargin{6};
    end
  end
  for k = optionStart:nargin
    option = lower(char(varargin{k}));
    if strcmp(option, 'onesided') || strcmp(option, 'twosided') || strcmp(option, 'centered') ...
        || strcmp(option, 'half') || strcmp(option, 'whole')
      range = option;
    else
      error('Nelson:signal:unsupportedOption', 'Unsupported option.');
    end
  end
  [pxy, f] = signalWelchCore(x, y, win, overlap, nfft, fs, range, 'psd');
  if nargout == 0
    plot(f, pow2db(abs(pxy)));
    return
  end
  varargout{1} = pxy;
  if nargout > 1
    varargout{2} = f;
  end
end
%=============================================================================
