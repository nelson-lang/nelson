%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = range(x, dim)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if ~isnumeric(x) && ~islogical(x)
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Input');
  end
  if nargin < 2
    y = max(x) - min(x);
  else
    y = max(x, [], dim) - min(x, [], dim);
  end
end
%=============================================================================
