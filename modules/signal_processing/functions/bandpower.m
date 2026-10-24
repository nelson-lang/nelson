%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = bandpower(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  for k = 1:nargin
    if isinteger(varargin{k})
      % power is a double quantity: integer arithmetic would saturate
      varargin{k} = double(varargin{k});
    end
  end
  if isPsdInput(varargin)
    pxx = varargin{1};
    f = varargin{2};
    if nargin >= 4
      freqRange = varargin{3};
    else
      freqRange = [];
    end
    p = signalBandpowerFromPsd(pxx, f, freqRange);
    return;
  end
  x = varargin{1};
  if nargin == 1
    p = mean(abs(x(:)) .^ 2);
  elseif nargin == 3
    fs = varargin{2};
    freqRange = varargin{3};
    [pxx, f] = signalPeriodogramCore(x, hamming(length(x)), length(x), fs);
    p = signalBandpowerFromPsd(pxx, f, freqRange);
  else
    error('Nelson:signal:bandpower:missingSampleRate', 'Frequency range must be specified with sampling frequency.');
  end
end
%=============================================================================
function tf = isPsdInput(args)
  tf = numel(args) >= 3 && (ischar(args{end}) || isstring(args{end})) && strcmpi(char(args{end}), 'psd');
end
%=============================================================================
