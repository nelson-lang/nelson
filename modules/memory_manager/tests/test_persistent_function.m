%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = test_persistent_function()
  persistent calls;
  if isempty(calls)
    calls = 0;
  end
  disp(['nb calls to test_persistent_function: ', int2str(calls)]);
  r= calls;
  calls = calls + 1;
end
