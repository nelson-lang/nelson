%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function signalValidateWindowLength(m)
  isPositiveIntegerOrEmpty = isempty(m) || (isscalar(m) && (m == fix(m)) && (m > 0));
  if ~isPositiveIntegerOrEmpty
    error('Nelson:validators:mustBePositiveInteger', '%s must be a positive integer.', 'M');
  end
end
%=============================================================================
