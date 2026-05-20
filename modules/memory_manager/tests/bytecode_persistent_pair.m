%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = bytecode_persistent_pair()
  persistent leftValue rightValue
  if isempty(leftValue)
    leftValue = 0;
  end
  if isempty(rightValue)
    rightValue = 100;
  end
  leftValue = leftValue + 1;
  rightValue = rightValue + 10;
  v = [leftValue rightValue];
end
%=============================================================================
