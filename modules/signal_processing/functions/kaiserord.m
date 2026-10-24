%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = kaiserord(varargin)
  narginchk(3, 4);
  nargoutchk(0, 4);
  f = varargin{1};
  a = varargin{2};
  dev = varargin{3};
  fs = 2;
  if nargin == 4
    fs = varargin{4};
  end
  f = f(:).' ./ (fs / 2);
  a = a(:).';
  dev = dev(:).';
  width = min(diff(f));
  attenuation = -20 * log10(min(dev));
  if attenuation > 50
    beta = 0.1102 * (attenuation - 8.7);
  elseif attenuation >= 21
    beta = 0.5842 * (attenuation - 21) ^ 0.4 + 0.07886 * (attenuation - 21);
  else
    beta = 0;
  end
  n = ceil((attenuation - 8) / (2.285 * pi * width));
  if length(f) == 2
    wn = mean(f);
  else
    wn = (f(1:2:end - 1) + f(2:2:end)) / 2;
  end
  if length(a) == 2 && a(1) > a(2)
    ftype = 'low';
  elseif length(a) == 2 && a(1) < a(2)
    ftype = 'high';
  elseif a(1) == 0
    ftype = 'DC-0';
  else
    ftype = 'stop';
  end
  if strcmp(ftype, 'high') || strcmp(ftype, 'stop')
    if mod(n, 2) ~= 0
      n = n + 1;
    end
  else
    if mod(n, 2) == 0
      n = n + 1;
    end
  end
  varargout{1} = n;
  if nargout > 1
    varargout{2} = wn;
  end
  if nargout > 2
    varargout{3} = beta;
  end
  if nargout > 3
    varargout{4} = ftype;
  end
end
%=============================================================================
