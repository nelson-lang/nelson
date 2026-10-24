%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pair = signalTakePair(values, section)
  first = 2 * section - 1;
  last = min(2 * section, length(values));
  if first > length(values)
    pair = [];
  else
    pair = values(first:last);
  end
end
%=============================================================================
