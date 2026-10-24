%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = toCellstrRow(obj, names)
  if isempty(names)
    names = {};
  elseif isstring(names)
    names = cellstr(names);
  elseif ischar(names)
    names = {names};
  end
  names = names(:)';
end
%=============================================================================
