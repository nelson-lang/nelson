%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function location = jsonTableColumnar(opts, tableNodeName, selectorSet)
  % Arguments of __jsondecode_columns__ locating the table node as
  % jsonTableDetect does (TableSelector when 'selectorSet', else
  % TableNodeName, else the automatic search), or {} when the table cannot
  % be read from decoded columns: metadata selector, variables or row names
  % not selected by top-level keys, TableSelector not a JSON Pointer.
  location = {};
  if ~isempty(opts.VariableUnitsSelector) || ~isempty(opts.VariableDescriptionsSelector) ...
      || ~(isempty(opts.RowNamesSelector) || isTopLevel(opts.RowNamesSelector))
    return
  end
  selectors = opts.VariableSelectors;
  for k = 1:numel(selectors)
    if ~isTopLevel(selectors{k})
      return
    end
  end
  selector = char(opts.TableSelector);
  if selectorSet && isempty(selector)
    location = {'root'};
  elseif selectorSet
    if selector(1) == '/'
      location = {'pointer', jsonPointerTokens(selector)};
    end
  elseif ~isempty(tableNodeName)
    location = {'name', tableNodeName};
  else
    location = {'auto'};
  end
end
%=============================================================================
function tf = isTopLevel(pointer)
  pointer = char(pointer);
  tf = strcmp(pointer, 'Keys') || (numel(pointer) > 1 && pointer(1) == '/' && ~any(pointer(2:end) == '/'));
end
%=============================================================================
