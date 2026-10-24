%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = tableDotIndexSubsasgn(obj, sasgn, value)
  % T.name(indices) = value and T.name{indices} = value, T.(n) naming the
  % variable number n. The variable is indexed as an array: a name that is
  % not a variable yet starts as a column of the default value of the class
  % of value (an empty cell for braces), and rows assigned past the height
  % are added to the table.
  % Properties and the dimension names are read, indexed and assigned back.
  st = struct(obj);
  names = st.Properties.VariableNames;
  if ~ischar(sasgn(1).subs)
    sasgn = tableNumberedDot(names, sasgn, true);
  end
  field = sasgn(1).subs;
  isVariable = any(strcmp(names, field));
  if ~isVariable && (strcmp(field, 'Properties') || any(strcmp(st.Properties.DimensionNames, field)))
    obj = subsasgn(obj, sasgn(1), subsasgn(subsref(obj, sasgn(1)), sasgn(2), value));
    return
  end
  nRows = height(obj);
  if isVariable
    column = st.data.(field);
  elseif strcmp(sasgn(2).type, '{}')
    column = tableDefaultColumn({[]}, nRows);
  elseif isempty(value)
    error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), field);
  else
    column = tableDefaultColumn(value(1), nRows);
  end
  column = subsasgn(column, sasgn(2), value);
  if size(column, 1) <= nRows
    obj = subsasgn(obj, sasgn(1), column);
    return
  end
  if isempty(names)
    st.data = struct();
  end
  st.data.(field) = column;
  if ~isVariable
    st.Properties.VariableNames{end + 1} = field;
  end
  st.Properties.VariableTypes = string([]);
  obj = table.fromStruct(tableGrowRows(st, nRows, {field}, {}));
end
%=============================================================================
