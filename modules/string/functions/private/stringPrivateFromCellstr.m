%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = stringPrivateFromCellstr(items, kind, charAsCell)
  if nargin < 3
    charAsCell = false;
  end
  switch kind
    case 'string'
      out = string(items);
    case 'cell'
      out = items;
    otherwise
      if charAsCell
        out = items;
      elseif isempty(items)
        out = '';
      elseif isscalar(items)
        out = items{1};
      else
        out = char(items);
      end
  end
end
%=============================================================================
