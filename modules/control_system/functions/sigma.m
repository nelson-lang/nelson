%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = sigma(varargin)
  narginchk(1, 2);
  nargoutchk(0, 2);
  sys = varargin{1};
  if nargin == 2
    w = varargin{2};
  else
    w = logspace(-2, 2, 1000);
  end
  H = freqresp(sys, w);
  nFreq = length(w);
  nValues = min(size(H, 1), size(H, 2));
  sv = zeros(nValues, nFreq);
  for k = 1:nFreq
    sv(:, k) = svd(H(:, :, k));
  end
  if nargout == 0
    semilogx(w, 20 * log10(sv));
    xlabel(_('Frequency [rad/s]'));
    ylabel(_('Singular Values [dB]'));
    varargout = {};
  else
    varargout{1} = sv;
    if nargout > 1
      varargout{2} = w;
    end
  end
end
%=============================================================================
