%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function userData = ltiMergeUserData(left, right)
  userData = [];
  if ~isempty(left.UserData) && ~isempty(right.UserData)
    userData = right.UserData;
    return
  end
  if ~isempty(left.UserData)
    userData = left.UserData;
    return
  end
  if ~isempty(right.UserData)
    userData = right.UserData;
  end
end
%=============================================================================
