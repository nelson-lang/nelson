%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function b = fir2(varargin)
  narginchk(3, 4);
  nargoutchk(0, 1);
  n = varargin{1};
  f = varargin{2};
  m = varargin{3};
  if nargin == 4
    win = varargin{4};
  else
    win = hamming(n + 1);
  end
  mustBeInteger(n);
  mustBeNonnegative(n);
  f = f(:).';
  m = m(:).';
  if length(f) ~= length(m)
    error('Nelson:dimagree:valueLength', '%s must have the same length.', 'Frequency and magnitude vectors');
  end
  gridCount = max(1024, 2 ^ nextpow2(2 * (n + 1)));
  grid = (0:gridCount / 2) / (gridCount / 2);
  mag = interp1(f, m, grid, 'linear');
  phase = exp(-complex(0, 1) * pi * grid * n / 2);
  response = mag .* phase;
  spectrum = [response, conj(response(end - 1:-1:2))];
  h = real(ifft(spectrum));
  b = h(1:n + 1) .* win(:).';
end
%=============================================================================
