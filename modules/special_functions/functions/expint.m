%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = expint(X)
  % exponential integral E1 for real or complex X
  narginchk(1, 1);
  if ~isfloat(X)
    error('Nelson:expint:invalidType', 'X must be a single or double array.');
  end
  if strcmp(class(X), 'single')
    Y = single(expint(double(X)));
  elseif isreal(X) && ~any(X(:) < 0)
    Y = nelson.special_functions.internal.expintCore(X);
  else
    Y = nelson.special_functions.internal.expintCore(complex(X));
  end
end
%=============================================================================
