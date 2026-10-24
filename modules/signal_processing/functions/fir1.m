%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function b = fir1(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  n = varargin{1};
  wn = varargin{2};
  ftype = '';
  win = [];
  for k = 3:nargin
    value = varargin{k};
    if ischar(value) || isstring(value)
      ftype = char(value);
    else
      win = value;
    end
  end
  mustBeInteger(n);
  mustBeNonnegative(n);
  if isempty(win)
    win = hamming(n + 1);
  else
    win = win(:);
  end
  if length(win) ~= n + 1
    error('Nelson:signal:fir1:invalidWindowLength', 'Window length must be filter order plus one.');
  end
  if isempty(ftype)
    if length(wn) == 1
      ftype = 'low';
    else
      ftype = 'bandpass';
    end
  end
  m = (0:n) - n / 2;
  if length(wn) == 1
    wc = wn(1);
    h = wc * sinc(wc * m);
    if strcmp(ftype, 'high')
      h = -h;
      h(n / 2 + 1) = h(n / 2 + 1) + 1;
    end
  else
    w1 = wn(1);
    w2 = wn(2);
    h = w2 * sinc(w2 * m) - w1 * sinc(w1 * m);
    if strcmp(ftype, 'stop')
      h = -h;
      h(n / 2 + 1) = h(n / 2 + 1) + 1;
    end
  end
  b = h(:).' .* win(:).';
  if strcmp(ftype, 'high')
    scale = abs(sum(b .* ((-1) .^ (0:n))));
  elseif strcmp(ftype, 'bandpass')
    center = mean(wn);
    scale = abs(sum(b .* exp(-complex(0, 1) * pi * center * (0:n))));
  else
    scale = sum(b);
  end
  if scale ~= 0
    b = real(b ./ scale);
  end
end
%=============================================================================
