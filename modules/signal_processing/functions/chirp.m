%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = chirp(varargin)
  narginchk(1, 6);
  nargoutchk(0, 1);
  t = varargin{1};
  f0 = 0;
  t1 = 1;
  f1 = 100;
  method = 'linear';
  phi = 0;
  if nargin >= 2
    f0 = varargin{2};
  end
  if nargin >= 3
    t1 = varargin{3};
  end
  if nargin >= 4
    f1 = varargin{4};
  end
  if nargin == 5
    method = varargin{5};
  end
  if nargin == 6
    phi = varargin{6};
  end
  if strcmpi(method, 'quadratic')
    beta = (f1 - f0) / (t1 ^ 2);
    phase = 2 * pi * (f0 * t + beta * t .^ 3 / 3);
  elseif strcmpi(method, 'logarithmic')
    if f0 * f1 <= 0
      error('Nelson:signal:chirp:invalidFrequencies', 'Frequencies must be nonzero and have the same sign for logarithmic sweeps.');
    end
    ratio = f1 / f0;
    phase = 2 * pi * f0 * t1 / log(ratio) * (ratio .^ (t / t1) - 1);
  else
    beta = (f1 - f0) / t1;
    phase = 2 * pi * (f0 * t + beta * t .^ 2 / 2);
  end
  phase = phase + phi * pi / 180;
  y = cos(phase);
end
%=============================================================================
