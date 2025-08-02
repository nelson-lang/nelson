%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = factorial(X)
  if ~isreal(X) || any(fix(X(:)) ~= X(:)) || any(X(:) < 0)
    error(_('#1 input argument: must be real positive integer values.'));
  end
  if isinteger(X)
    Y = round(gamma(single(X + 1)));
  else
    Y = round(gamma(X + 1));
  end
  if ~issingle(X) && ~isdouble(X)
    Y = cast(Y, 'like', X);
  end
end
%=============================================================================
