%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = graphicsNumericColumn(value, name)
  if ~isnumeric(value)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', name);
  end
  value = value(:);
end
%=============================================================================
