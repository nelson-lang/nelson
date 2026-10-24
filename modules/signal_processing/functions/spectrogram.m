%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = spectrogram(varargin)
  narginchk(1, 5);
  nargoutchk(0, 4);
  x = varargin{1}(:);
  if nargin >= 2 && ~isempty(varargin{2})
    window = varargin{2};
    if isscalar(window)
      window = hamming(window);
    else
      window = window(:);
    end
  else
    window = hamming(256);
  end
  if nargin >= 3 && ~isempty(varargin{3})
    overlap = varargin{3};
  else
    overlap = floor(length(window) / 2);
  end
  if nargin >= 4 && ~isempty(varargin{4})
    nfft = varargin{4};
  else
    nfft = max(256, 2 ^ nextpow2(length(window)));
  end
  if nargin == 5
    fs = varargin{5};
  else
    fs = 1;
  end
  [s, f, t] = signalSpectrogramCore(x, window, overlap, nfft, fs);
  p = abs(s) .^ 2 ./ (fs * sum(abs(window) .^ 2));
  if size(p, 1) > 2
    if mod(nfft, 2) == 0
      p(2:end - 1, :) = 2 * p(2:end - 1, :);
    else
      p(2:end, :) = 2 * p(2:end, :);
    end
  end
  if nargout == 0
    imagesc(t, f, pow2db(p));
  else
    varargout{1} = s;
    if nargout > 1
      varargout{2} = f;
    end
    if nargout > 2
      varargout{3} = t;
    end
    if nargout > 3
      varargout{4} = p;
    end
  end
end
%=============================================================================
