%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = select_matchesText(value, expected)
  tf = isempty(expected);
  if ~tf
    expected = cellstr(string(expected));
    value = char(string(value));
    tf = any(strcmp(expected, value));
  end
end
