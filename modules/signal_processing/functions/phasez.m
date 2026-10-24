%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = phasez(varargin)
  narginchk(1, 5);
  nargoutchk(0, 2);
  [h, w] = freqz(varargin{:});
  p = signalUnwrapPhase(angle(h));
  p(abs(h) <= 100 * eps() * max(1, max(abs(h)))) = NaN;
  if nargout == 0
    plot(w, p);
  else
    varargout{1} = p;
    if nargout > 1
      varargout{2} = w;
    end
  end
end
%=============================================================================
