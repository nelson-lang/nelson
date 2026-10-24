%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filename = parseOutputFile(param, msg)
  if ischar(param)
    filename = param;
  else
    error('Nelson:tests_manager:invalidOutputFile', _(msg));
  end
end
