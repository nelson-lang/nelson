%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = tscollectionValidName(name)
  if isempty(name)
    name = 'series';
  end
  chars = char(name);
  for k = 1:length(chars)
    ok = (chars(k) >= 'a' && chars(k) <= 'z') ...
      || (chars(k) >= 'A' && chars(k) <= 'Z') ...
      || (chars(k) >= '0' && chars(k) <= '9') ...
      || chars(k) == '_';
    if ~ok
      chars(k) = '_';
    end
  end
  if ~((chars(1) >= 'a' && chars(1) <= 'z') || (chars(1) >= 'A' && chars(1) <= 'Z'))
    chars = ['x', chars];
  end
  name = chars;
end
%=============================================================================
