%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = validateTextScalar(name, value)
  if isStringScalar(value)
    value = char(value);
  end
  if ~ischar(value) || size(value, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
end
%=============================================================================
