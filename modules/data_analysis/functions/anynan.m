%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = anynan(A)
  % true when A holds at least one NaN value
  narginchk(1, 1);
  if ~(isnumeric(A) || islogical(A) || ischar(A))
    error('Nelson:anynan:invalidType', 'Input must be numeric, char or logical.');
  end
  if isfloat(A)
    tf = any(isnan(A(:)));
  else
    tf = false;
  end
end
%=============================================================================
