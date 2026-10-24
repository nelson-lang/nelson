%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = select_hasTags(actual, expected)
  tf = false;
  if isempty(expected)
    return
  end
  expected = cellstr(string(expected));
  for k = 1:length(expected)
    tf = tf || any(strcmp(actual, expected{k}));
  end
end
