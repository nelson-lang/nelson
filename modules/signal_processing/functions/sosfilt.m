%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = sosfilt(sos, x, varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  if ~ismatrix(sos) || size(sos, 2) ~= 6
    error('Nelson:signal:invalidSOSMatrix', 'SOS matrix must have six columns.');
  end
  dim = [];
  if nargin == 3
    dim = varargin{1};
  end
  y = x;
  for row = 1:size(sos, 1)
    if isempty(dim)
      y = filter(sos(row, 1:3), sos(row, 4:6), y);
    else
      y = filter(sos(row, 1:3), sos(row, 4:6), y, [], dim);
    end
  end
end
%=============================================================================
