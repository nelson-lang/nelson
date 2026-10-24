%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stoponfail = parseStopOnFail(param)
  stoponfail = false;
  if isequal(size(param), [0 0]) && isdouble(param)
    return
  end
  stoponfail = haveStopOnFailOption(param);
  if ~stoponfail
    error('Nelson:tests_manager:invalidArgument:nArgumentStoponfailExpected', '#%d argument: ''-stoponfail'' or [] expected', 4);
  end
end
