%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = select_matchesKind(value, expected)
  tf = isempty(expected);
  if tf
    return
  end
  expected = cellstr(string(expected));
  value = char(string(value));
  tf = false;
  for k = 1:length(expected)
    kind = expected{k};
    tf = tf || strcmp(kind, 'all') ...
      || (strcmp(kind, 'all_tests') && (strcmp(value, 'test') || strcmp(value, 'bug'))) ...
      || strcmp(kind, value);
  end
end
