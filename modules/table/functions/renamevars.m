%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = renamevars(varargin)
  narginchk(3, 3); % Check that there are exactly 3 input arguments
  nargoutchk(0, 1); % Check that there is at most 1 output argument

  % Unpack input arguments
  T = varargin{1};
  oldNames = varargin{2};
  newNames = varargin{3};

  % Ensure the types are correct
  mustBeA(T, {'table', 'timetable'}, 1); % Check if T is a table or a timetable
  mustBeText(oldNames, 2); % Check if oldNames are text
  mustBeText(newNames, 3); % Check if newNames are text

  % Convert oldNames and newNames to cell arrays of strings
  oldNames = cellstr(oldNames);
  newNames = cellstr(newNames);

  % Ensure oldNames and newNames are the same length
  if (length(oldNames) ~= length(newNames))
    error('Nelson:table:dimagree:oldNamesNewNamesLength', 'oldNames and newNames must have the same length.');
  end

  % Check for duplicates in oldNames
  if (length(unique(oldNames)) ~= length(oldNames))
    error('Nelson:table:duplicateNames', 'Duplicate names in %s are not allowed.', 'oldNames');
  end

  % Check for duplicates in newNames
  if (length(unique(newNames)) ~= length(newNames))
    error('Nelson:table:duplicateNames', 'Duplicate names in %s are not allowed.', 'newNames');
  end

  if istimetable(T)
    varargout{1} = renameTimetableVariables(T, oldNames, newNames);
    return
  end

  % Convert table to struct
  st = struct(T);

  % Get current fieldnames from st.data
  currentFields = fieldnames(st.data);

  % Check if all oldNames exist in the current fieldnames
  for i = 1:length(oldNames)
    if ~any(strcmp(oldNames{i}, currentFields))
      error('Nelson:table:nameNotFound', 'Name "%s" not found in the table.', oldNames{i});
    end
  end

  % Rebuild the data struct in a single pass. Assigning newData.(name) one
  % field at a time reallocates the whole struct on every add, which made
  % renaming a wide table O(numVars^2).
  vals = struct2cell(st.data);
  renamedFields = currentFields;
  [tf, loc] = ismember(currentFields, oldNames);
  renamedFields(tf) = newNames(loc(tf));
  % Reject names that collide after renaming before rebuilding the struct, so
  % the table error is reported instead of a lower-level duplicate-field error.
  if length(unique(renamedFields)) ~= length(renamedFields)
    error('Nelson:table:duplicateVariableNames', _('%s names must be nonempty and distinct.'), 'Variable');
  end
  st.data = __tableStructFromCell__(vals, renamedFields);

  % Update VariableNames in Properties (vectorized for the same reason).
  oldVarNames = st.Properties.VariableNames;
  newVarNames = oldVarNames;
  [tf2, loc2] = ismember(oldVarNames, oldNames);
  newVarNames(tf2) = newNames(loc2(tf2));
  st.Properties.VariableNames = newVarNames;

  varargout{1} = table.fromStruct(st);
end
%=============================================================================
function TT = renameTimetableVariables(TT, oldNames, newNames)
  names = TT.Properties.VariableNames;
  for i = 1:length(oldNames)
    idx = find(strcmp(oldNames{i}, names));
    if isempty(idx)
      error('Nelson:table:nameNotFound', 'Name "%s" not found in the table.', oldNames{i});
    end
    names{idx} = newNames{i};
  end
  TT.Properties.VariableNames = names;
end
%=============================================================================
