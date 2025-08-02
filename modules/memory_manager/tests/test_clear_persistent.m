%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = test_clear_persistent()
  persistent val;
  if isempty(val)
    val = 0;
    r = val;
  else
    val = val + 1;
    r = val;
  end
end
%=============================================================================
