%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = freqz(varargin)
  narginchk(1, 5);
  nargoutchk(0, 2);

  b = varargin{1};
  a = 1;
  n = 512;
  fs = [];
  range = 'half';

  isSos = ismatrix(b) && ~isvector(b) && size(b, 2) == 6;
  if isSos
    [b, a] = sos2tf(b);
    argumentIndex = 2;
  elseif nargin >= 2
    a = varargin{2};
    argumentIndex = 3;
  else
    argumentIndex = 3;
  end

  if argumentIndex <= nargin && ~isempty(varargin{argumentIndex})
    if ischar(varargin{argumentIndex}) || isstring(varargin{argumentIndex})
      range = char(varargin{argumentIndex});
    else
      n = varargin{argumentIndex};
    end
    argumentIndex = argumentIndex + 1;
  end
  while argumentIndex <= nargin
    value = varargin{argumentIndex};
    if ischar(value) || isstring(value)
      range = char(value);
    else
      fs = value;
    end
    argumentIndex = argumentIndex + 1;
  end

  validateFilterVectors(b, a);

  if isscalar(n)
    nfft = double(n);
    if isempty(fs)
      if strcmp(range, 'whole')
        w = (0:nfft - 1)' * 2 * pi / nfft;
      else
        w = (0:nfft - 1)' * pi / nfft;
      end
    else
      if strcmp(range, 'whole')
        w = (0:nfft - 1)' * double(fs) / nfft;
      else
        w = (0:nfft - 1)' * double(fs) / (2 * nfft);
      end
    end
  else
    w = n(:);
  end

  if isempty(fs)
    evalw = w;
  else
    evalw = 2 * pi * w / double(fs);
  end
  h = signalFrequencyResponse(b, a, evalw);

  if nargout == 0
    plot(w, abs(h));
  else
    varargout{1} = h;
    if nargout > 1
      varargout{2} = w;
    end
  end
end
%=============================================================================
function validateFilterVectors(b, a)
  if isempty(a) || isempty(b) || ~isvector(a) || ~isvector(b)
    error('Nelson:signal:freqz:invalidCoefficients', 'Filter coefficients must be vectors.');
  end
  if a(1) == 0
    error('Nelson:signal:zeroLeadingDenominator', 'First denominator filter coefficient must be nonzero.');
  end
end
%=============================================================================
