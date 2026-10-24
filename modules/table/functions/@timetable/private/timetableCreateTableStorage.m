%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function st = timetableCreateTableStorage(varargin)
  st = struct();
  st.data = struct();
  st.Version = 2;
  st.VariableCustomProperties = {};
  st.Properties = defaultTableProperties();
  if nargin == 0
    return
  end

  if ischar(varargin{1}) && strcmpi(varargin{1}, 'Size')
    if nargin < 4 || ~ischar(varargin{3}) || ~strcmpi(varargin{3}, 'VariableTypes')
      error('Nelson:timetable:invalidSyntax:sizeVariableTypesSpecifiedTogether', 'Size and VariableTypes must be specified together.');
    end
    sz = varargin{2};
    variableTypes = toCellstrRow(varargin{4});
    nRows = sz(1);
    nVars = sz(2);
    variableNames = cell(1, nVars);
    for k = 1:nVars
      variableNames{k} = ['Var', num2str(k)];
      st.data.(variableNames{k}) = initialValue(nRows, variableTypes{k});
    end
    st.Properties.VariableNames = variableNames;
    st.Properties.VariableTypes = string(variableTypes);
    return
  end

  variableNames = cell(1, nargin);
  for k = 1:nargin
    variableNames{k} = ['Var', num2str(k)];
  end
  % Build the data struct in one pass: st.data.(name) = ... per variable grows
  % a nested struct field and recopies it on every assignment (O(nargin^2)).
  st.data = __tableStructFromCell__(varargin(:), variableNames(:));
  st.Properties.VariableNames = variableNames;
end

function props = defaultTableProperties()
  props = struct();
  props.DimensionNames = {'Row', 'Variables'};
  props.Description = '';
  props.UserData = [];
  props.VariableNames = {};
  props.VariableTypes = string([]);
  props.VariableDescriptions = {};
  props.VariableUnits = {};
  props.VariableContinuity = [];
  props.RowNames = {};
  props.CustomProperties = struct();
end

function names = toCellstrRow(names)
  if isempty(names)
    names = {};
  elseif isstring(names)
    names = cellstr(names);
  elseif ischar(names)
    names = {names};
  end
  names = names(:)';
end

function value = initialValue(nRows, typeName)
  switch lower(typeName)
    case {'double', 'doublenan'}
      value = zeros(nRows, 1);
      if contains(lower(typeName), 'nan')
        value(:) = NaN;
      end
    case 'single'
      value = single(zeros(nRows, 1));
    case 'logical'
      value = false(nRows, 1);
    case 'string'
      value = strings(nRows, 1);
    case {'cell', 'cellstr', 'char'}
      value = cell(nRows, 1);
      for k = 1:nRows
        value{k} = '';
      end
    case 'int8'
      value = int8(zeros(nRows, 1));
    case 'int16'
      value = int16(zeros(nRows, 1));
    case 'int32'
      value = int32(zeros(nRows, 1));
    case 'int64'
      value = int64(zeros(nRows, 1));
    case 'uint8'
      value = uint8(zeros(nRows, 1));
    case 'uint16'
      value = uint16(zeros(nRows, 1));
    case 'uint32'
      value = uint32(zeros(nRows, 1));
    case 'uint64'
      value = uint64(zeros(nRows, 1));
    otherwise
      error('Nelson:timetable:unsupportedVariableType', 'Unsupported variable type: %s', typeName);
  end
end
%=============================================================================
