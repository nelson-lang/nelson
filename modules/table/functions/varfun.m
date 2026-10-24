%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = varfun(fun, T, varargin)
  narginchk(2, Inf);
  isTimetable = isa(T, 'timetable');
  if ~(isa(T, 'table') || isTimetable)
    error('Nelson:table:invalidType:valueOneTheseTypesTableTimetable', 'Value must have one of these types: ''table'', ''timetable''.');
  end
  vars = 1:width(T);
  outputFormat = 'table';
  groupVars = [];
  st = struct(T);
  names = st.Properties.VariableNames;
  k = 1;
  while k <= length(varargin)
    option = char(varargin{k});
    switch lower(option)
      case 'inputvariables'
        vars = tableResolveVariables(names, varargin{k + 1}, st.Properties.VariableTypes, st.data);
      case 'outputformat'
        outputFormat = lower(char(varargin{k + 1}));
      case 'groupingvariables'
        groupVars = varargin{k + 1};
      otherwise
        error('Nelson:table:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  if ~isempty(groupVars)
    R = varfunGroupedLocal(fun, T, names, st, vars, groupVars);
    return
  end
  values = cell(1, length(vars));
  outNames = cell(1, length(vars));
  for j = 1:length(vars)
    name = names{vars(j)};
    values{j} = fun(T.(name));
    outNames{j} = [func2str(fun), '_', name];
  end
  switch outputFormat
    case 'table'
      if isTimetable
        rowTimes = T.Properties.RowTimes;
        R = timetable(rowTimes(1), values{:}, ...
          'VariableNames', tableMakeUniqueNames(outNames));
      else
        R = table(values{:}, 'VariableNames', tableMakeUniqueNames(outNames));
      end
    case 'uniform'
      R = [values{:}];
    case 'cell'
      R = values;
    otherwise
      error('Nelson:table:unsupportedOutputFormat', 'Unsupported "%s" option.', 'OutputFormat');
  end
end
%=============================================================================
function R = varfunGroupedLocal(fun, T, names, st, vars, groupVars)
  gIdx = tableResolveVariables(names, groupVars, st.Properties.VariableTypes, st.data);
  dataIdx = vars(~ismember(vars, gIdx));
  gcols = cell(1, numel(gIdx));
  for j = 1:numel(gIdx)
    gcols{j} = T.(names{gIdx(j)});
  end
  G = findgroups(gcols{:});
  Gv = G(:);
  finite = Gv(~isnan(Gv));
  if isempty(finite)
    ng = 0;
  else
    ng = max(finite);
  end
  firstRows = zeros(ng, 1);
  groupCount = zeros(ng, 1);
  for g = 1:ng
    firstRows(g) = find(Gv == g, 1);
    groupCount(g) = sum(Gv == g);
  end
  outCols = {};
  outNames = {};
  for j = 1:numel(gIdx)
    col = T.(names{gIdx(j)});
    outCols{end + 1} = col(firstRows, :); %#ok<AGROW>
    outNames{end + 1} = names{gIdx(j)}; %#ok<AGROW>
  end
  outCols{end + 1} = groupCount;
  outNames{end + 1} = 'GroupCount';
  for j = 1:numel(dataIdx)
    name = names{dataIdx(j)};
    outCols{end + 1} = splitapply(fun, T.(name), G); %#ok<AGROW>
    outNames{end + 1} = [func2str(fun), '_', name]; %#ok<AGROW>
  end
  R = table(outCols{:}, 'VariableNames', tableMakeUniqueNames(outNames));
end
%=============================================================================
