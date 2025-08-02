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
  mustBeA(T, 'table', 1); % Check if T is a table
  mustBeText(oldNames, 2); % Check if oldNames are text
  mustBeText(newNames, 3); % Check if newNames are text
  
  % Convert oldNames and newNames to cell arrays of strings
  oldNames = cellstr(oldNames);
  newNames = cellstr(newNames);
  
  % Ensure oldNames and newNames are the same length
  if (length(oldNames) ~= length(newNames))
    error(_('oldNames and newNames must have the same length.'));
  end
  
  % Check for duplicates in oldNames
  if (length(unique(oldNames)) ~= length(oldNames))
    error('Duplicate names in oldNames are not allowed.');
  end
  
  % Check for duplicates in newNames
  if (length(unique(newNames)) ~= length(newNames))
    error('Duplicate names in newNames are not allowed.');
  end
  
  % Convert table to struct
  st = struct(T);
  
  % Get current fieldnames from st.data
  currentFields = fieldnames(st.data);
  
  % Check if all oldNames exist in the current fieldnames
  for i = 1:length(oldNames)
    if ~any(strcmp(oldNames{i}, currentFields))
      error(sprintf(_('Name "%s" not found in the table.'), oldNames{i}));
    end
  end
  
  % Create new struct for renamed fields
  newData = struct();
  
  % Copy and rename fields
  for i = 1:length(currentFields)
    currentField = currentFields{i};
    idx = find(strcmp(currentField, oldNames));
    
    if ~isempty(idx)
      % If field should be renamed, use new name
      newData.(newNames{idx}) = st.data.(currentField);
    else
      % If field is not in oldNames, keep original name
      newData.(currentField) = st.data.(currentField);
    end
  end
  
  % Replace old data struct with new one
  st.data = newData;
  
  % Update VariableNames in Properties
  oldVarNames = st.Properties.VariableNames;
  newVarNames = oldVarNames;
  for i = 1:length(oldNames)
    idx = strcmp(oldNames{i}, oldVarNames);
    if any(idx)
      newVarNames{idx} = newNames{i};
    end
  end
  st.Properties.VariableNames = newVarNames;
  
  varargout{1} = class(st, 'table');
end
