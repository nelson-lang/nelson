%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = rms(varargin)
% Root Mean Square (RMS) of array elements.
%
%   y = rms(x)
%   y = rms(x, dim)
%   y = rms(x, 'all')
%   y = rms(x, dim, type)
%   y = rms(x, 'all', type)
%   y = rms(x, dim, type, nanflag)
%   y = rms(x, 'all', type, nanflag)
%
% Parameters:
%   x        - Input array (real or complex, any numeric type)
%   dim      - Dimension to operate along (positive integer scalar)
%   type     - 'default', 'double', or 'native'
%   nanflag  - 'includenan', 'omitnan', 'omitmissing', 'includemissing'
%
% Returns:
%   y        - Root mean square value(s)
%
% Example:
%   M = uint8([10:30:70;20:30:80;30:30:90]);
%   R = rms(M, 'native')
%
  x = varargin{1};
  if isinteger(x)
    x = double(x);
  end

  % Prepare arguments for mean
  mean_args = varargin(2:end);

  if isreal(x)
    y = sqrt(mean(x .* x, mean_args{:}));
  else
    y = sqrt(mean(real(x).^2 + imag(x).^2, mean_args{:}));
  end
end
%=============================================================================
