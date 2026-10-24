%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function text = stringPrivateApplyLiteralReplace(text, oldValue, newValue)
  olds = stringPrivateToReplacementList(oldValue);
  news = stringPrivateToReplacementList(newValue);
  if numel(news) ~= 1 && numel(news) ~= numel(olds)
    error('Nelson:string:replacementTextScalarSameNumber', 'Replacement text must be scalar or have the same number of elements as matched text.');
  end
  for k = 1:numel(olds)
    if numel(news) == 1
      currentNew = news{1};
    else
      currentNew = news{k};
    end
    text = replace(text, olds{k}, currentNew);
  end
end
%=============================================================================
function values = stringPrivateToReplacementList(value)
  if ischar(value)
    values = {value};
  elseif isstring(value) || iscellstr(value)
    values = cellstr(value);
  else
    error('Nelson:string:stringCharacterVectorCellArray', 'String, character vector, or cell array of character vectors expected.');
  end
end
%=============================================================================
