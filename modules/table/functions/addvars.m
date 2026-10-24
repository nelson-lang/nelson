%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Tout = addvars(T, varargin)
  narginchk(1, Inf);
  mustBeA(T, {'table', 'timetable'}, 1);
  % A variable passed by name keeps that name: the input names are only
  % known in this call, so timetables and eventtables are handled here too.
  argNames = cell(1, length(varargin));
  for k = 1:length(varargin)
    argNames{k} = inputname(k + 1);
  end
  Tout = localAddvars(T, varargin, argNames, T.Properties.DimensionNames);
end
%=============================================================================
function Tout = localAddvars(T, args, argNames, dimensionNames)
  if isa(T, 'eventtable')
    TT = localAddvars(eventtable.toTimetable(T), args, argNames, dimensionNames);
    Tout = eventtable.fromTimetable(TT, eventtable.variables(T));
    return
  end
  if istimetable(T)
    % The variables are added to the table storage; the row times and the
    % timetable properties are kept.
    st = struct(T);
    R = localAddvars(table.fromStruct(st.Table), args, argNames, dimensionNames);
    Tout = timetable.fromTableAndTimes(R, T.Properties.RowTimes, T.Properties);
    return
  end
  % The name-value arguments start at the first character vector, a string
  % is always a value to add.
  nValues = length(args);
  for k = 1:length(args)
    if ischar(args{k}) && size(args{k}, 1) <= 1
      nValues = k - 1;
      break
    end
  end
  values = args(1:nValues);
  st = struct(T);
  names = st.Properties.VariableNames;
  [insertAt, newNames] = tableAddMoveOptions(names, args(nValues + 1:end), true);
  if iscell(newNames)
    localCheckNewNames(names, newNames, nValues);
  else
    newNames = localDefaultNames([names, dimensionNames], argNames(1:nValues), length(names));
  end

  % Append the new variables in one pass. st.data.(newNames{k}) = ... per
  % variable grows the nested data struct and recopies it each time, which
  % made adding many variables at once O(numVars^2).
  if isempty(st.data) || isempty(fieldnames(st.data))
    existingNames = {};
    existingVals = {};
  else
    existingNames = fieldnames(st.data);
    existingVals = struct2cell(st.data);
  end
  st.data = __tableStructFromCell__([existingVals(:); values(:)], [existingNames(:); newNames(:)]);
  st.Properties.VariableNames = [names(1:insertAt - 1), newNames, names(insertAt:end)];
  st.data = localReorderData(st.data, st.Properties.VariableNames);
  Tout = table.fromStruct(st);
end
%=============================================================================
function newNames = localDefaultNames(usedNames, argNames, width)
  % The input name, or Var<N> numbered after the existing variables for an
  % expression; a name already used by the table or by another new variable
  % is suffixed _1, _2, ...
  nValues = length(argNames);
  baseNames = argNames;
  for k = 1:nValues
    if isempty(baseNames{k})
      baseNames{k} = ['Var', num2str(width + k)];
    end
  end
  newNames = cell(1, nValues);
  for k = 1:nValues
    candidate = baseNames{k};
    if any(strcmp([usedNames, newNames(1:k - 1)], candidate))
      reserved = [usedNames, baseNames, newNames(1:k - 1)];
      suffix = 1;
      candidate = [baseNames{k}, '_', num2str(suffix)];
      while any(strcmp(reserved, candidate))
        suffix = suffix + 1;
        candidate = [baseNames{k}, '_', num2str(suffix)];
      end
    end
    newNames{k} = candidate;
  end
end
%=============================================================================
function localCheckNewNames(names, newNames, nValues)
  if length(newNames) ~= nValues
    error('Nelson:table:incorrectNumberOfVarNames', _('Specify one new variable name for each variable being added to the table.'));
  end
  if any(cellfun('isempty', newNames))
    error('Nelson:table:zeroLengthVarname', _('Table variable names must be nonempty text.'));
  end
  allNames = [names, newNames];
  for k = 1:nValues
    if sum(strcmp(allNames, newNames{k})) > 1
      error('Nelson:table:duplicateVariableNames', _('Duplicate table variable name: ''%s''.'), newNames{k});
    end
  end
end
%=============================================================================
function data = localReorderData(data, variableNames)
  % Reorder the data in a single pass. Assigning reordered.(name) one field
  % at a time reallocates the whole struct on every add, which made adding a
  % variable to a wide table O(numVars^2).
  existing = fieldnames(data);
  vals = struct2cell(data);
  [~, loc] = ismember(variableNames(:), existing);
  data = __tableStructFromCell__(vals(loc), variableNames(:));
end
%=============================================================================
