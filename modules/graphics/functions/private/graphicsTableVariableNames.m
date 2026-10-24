%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = graphicsTableVariableNames(tbl, selector)
  allNames = tbl.Properties.VariableNames;
  if ischar(selector) || isStringScalar(selector)
    names = cellstr(selector);
  elseif isstring(selector)
    names = cellstr(selector(:)');
  elseif iscellstr(selector)
    names = selector(:)';
  elseif isnumeric(selector)
    selector = double(selector(:)');
    names = cell(1, numel(selector));
    for k = 1:numel(selector)
      index = selector(k);
      if index < 1 || index > numel(allNames) || floor(index) ~= index
        error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
      end
      names{k} = allNames{index};
    end
  elseif islogical(selector) && isvector(selector)
    if numel(selector) ~= numel(allNames)
      error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
    end
    names = allNames(selector(:)');
  else
    error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
  end
  for k = 1:numel(names)
    if ~any(strcmp(names{k}, allNames))
      error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
    end
  end
end
%=============================================================================
