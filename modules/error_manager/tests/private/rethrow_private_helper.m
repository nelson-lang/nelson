%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rethrow_private_helper()
  try
    error('Nelson:test:privateRethrow', 'Private rethrow message.')
  catch ex
    rethrow(ex)
  end
end
%=============================================================================
