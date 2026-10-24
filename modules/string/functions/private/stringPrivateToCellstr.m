%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [items, shape, kind] = stringPrivateToCellstr(value)
  shape = size(value);
  if ischar(value)
    items = {value};
    shape = [1 1];
    kind = 'char';
  elseif isstring(value)
    items = cellstr(value);
    kind = 'string';
  elseif iscellstr(value)
    items = value;
    kind = 'cell';
  else
    error('Nelson:string:stringCharacterVectorCellArray', 'String, character vector, or cell array of character vectors expected.');
  end
end
%=============================================================================
