%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = rescale(X, varargin)
  narginchk(1, Inf);
  a = 0;
  b = 1;
  args = varargin;
  np = 0;
  while np < numel(args) && np < 2 && isnumeric(args{np + 1}) && isscalar(args{np + 1})
    np = np + 1;
  end
  if np >= 1
    a = args{1};
  end
  if np >= 2
    b = args{2};
  end
  args = args(np + 1:end);
  inMin = min(X(:));
  inMax = max(X(:));
  for i = 1:2:numel(args)
    name = lower(char(args{i}));
    value = args{i + 1};
    switch name
      case 'inputmin'
        inMin = value;
      case 'inputmax'
        inMax = value;
      otherwise
        error('Nelson:data_analysis:unknownOption', 'Unknown option.');
    end
  end
  range = inMax - inMin;
  if range == 0
    Y = repmat(a, size(X));
  else
    Y = a + (X - inMin) .* (b - a) ./ range;
  end
end
%=============================================================================
