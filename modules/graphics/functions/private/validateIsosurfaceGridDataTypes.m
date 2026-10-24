%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateIsosurfaceGridDataTypes(X, Y, Z)
  if ~isnumeric(X) || ~isreal(X) || ~isnumeric(Y) || ~isreal(Y) || ~isnumeric(Z) || ~isreal(Z)
    error('Nelson:validateIsosurfaceGridDataTypes:invalidGridData', 'Grid data must be real numeric.');
  end
end
%=============================================================================
