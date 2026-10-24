%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = medfreq(varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  [pxx, f, freqRange] = parseMeanMedInputs(varargin{:});
  [fm, pwr] = signalMedfreqFromPsd(pxx, f, freqRange);
  varargout{1} = fm;
  if nargout > 1
    varargout{2} = pwr;
  end
end
%=============================================================================
function [pxx, f, freqRange] = parseMeanMedInputs(varargin)
  freqRange = [];
  x = varargin{1};
  if nargin >= 2 && ~isscalar(varargin{2})
    pxx = x;
    f = varargin{2};
    if nargin == 3
      freqRange = varargin{3};
    end
  else
    fs = 1;
    if nargin >= 2
      fs = varargin{2};
    end
    [pxx, f] = signalPeriodogramCore(x, [], length(x), fs);
  end
end
%=============================================================================
