%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Tout = movevars(T, vars, varargin)
  narginchk(2, Inf);
  mustBeA(T, 'table', 1);
  st = struct(T);
  origNames = st.Properties.VariableNames;
  nvars = length(origNames);
  insertAt = tableAddMoveOptions(origNames, varargin, false);
  moveIdx = tableResolveVariableSubscript(origNames, vars, 'Nelson:table:vartypeInvalidVars', ...
    _('Using a vartype subscripter to specify variables to move is not supported. Use a character vector, string array, cell array of character vectors, or numeric or logical indices.'));

  % The location refers to the input table: the variables left in place
  % keep their side of it, the moved ones go in between in the given order.
  remainingIdx = setdiff(1:nvars, moveIdx, 'stable');
  newOrder = [remainingIdx(remainingIdx < insertAt), moveIdx, remainingIdx(remainingIdx >= insertAt)];
  newNames = localUniqueCopyNames(origNames(newOrder), newOrder);

  % Reorder every per-variable property array (not just the names) so they
  % stay aligned with the moved data.
  perVarFields = {'VariableTypes', 'VariableDescriptions', 'VariableUnits', 'VariableContinuity'};
  for f = 1:length(perVarFields)
    field = perVarFields{f};
    if isfield(st.Properties, field) && length(st.Properties.(field)) == nvars
      st.Properties.(field) = st.Properties.(field)(newOrder);
    end
  end
  st.Properties.CustomProperties = table.variableCustomPropertiesSubset(st, newOrder);
  st.Properties.VariableNames = newNames;
  % Reorder the data in a single pass. Assigning data.(name) one field at a
  % time reallocates the whole struct on every add, which made moving a
  % variable in a wide table O(numVars^2).
  vals = struct2cell(st.data);
  st.data = __tableStructFromCell__(vals(newOrder), newNames);
  Tout = table.fromStruct(st);
end
%=============================================================================
function names = localUniqueCopyNames(names, order)
  % A variable moved twice is copied under the name suffixed _1, _2, ...
  for k = 1:length(order)
    if any(order(1:k - 1) == order(k))
      base = names{k};
      suffix = 1;
      while any(strcmp(names, [base, '_', num2str(suffix)]))
        suffix = suffix + 1;
      end
      names{k} = [base, '_', num2str(suffix)];
    end
  end
end
%=============================================================================
