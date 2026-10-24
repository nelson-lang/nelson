%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = summary(A, varargin)
  narginchk(1, 2);
  if istable(A)
    narginchk(1, 1);
    S = localTableSummary(A);
    if nargout > 0
      varargout{1} = S;
    else
      disp(S);
    end
    return
  end
  if iscategorical(A)
    if nargout > 0
      varargout{1} = localCategoricalSummaryStruct(A);
    else
      localCategoricalSummary(A, varargin{:});
    end
    return
  end
  error('Nelson:data_analysis:inputTableCategoricalArray', 'Input must be a table or categorical array.');
end
%=============================================================================
function S = localTableSummary(T)
  st = struct(T);
  names = st.Properties.VariableNames;
  if isempty(names)
    S = struct();
    return
  end
  % Read the variables by position and build the result in one pass.
  % st.data.(name) looks a field up with a linear scan and S.(name)=info adds
  % a field at a time, so a per-variable loop would be O(numVars^2).
  vals = struct2cell(st.data);
  [~, loc] = ismember(names, fieldnames(st.data));
  infos = cell(length(names), 1);
  for k = 1:length(names)
    name = names{k};
    value = vals{loc(k)};
    info = struct();
    info.Size = size(value);
    info.Type = class(value);
    info.Description = localMetadata(st.Properties, 'VariableDescriptions', k, '');
    info.Units = localMetadata(st.Properties, 'VariableUnits', k, '');
    info.Continuity = localMetadata(st.Properties, 'VariableContinuity', k, []);
    if islogical(value)
      info.True = sum(value(:) == true);
      info.False = sum(value(:) == false);
    elseif isnumeric(value)
      numericValue = double(value(:));
      if isfloat(value)
        missing = isnan(numericValue);
      else
        missing = false(size(numericValue));
      end
      validValue = numericValue(~missing);
      info.NumMissing = sum(missing);
      if isempty(validValue)
        info.Min = [];
        info.Median = [];
        info.Max = [];
        info.Mean = [];
        info.Std = [];
      else
        info.Min = min(validValue);
        info.Median = localMedian(validValue);
        info.Max = max(validValue);
        info.Mean = mean(validValue);
        info.Std = localStd(validValue);
      end
    end
    infos{k} = info;
  end
  S = __tableStructFromCell__(infos, names(:));
end
%=============================================================================
function S = localCategoricalSummaryStruct(A)
  S = struct();
  S.Size = size(A);
  S.Type = 'categorical';
  S.Categories = A.categoryNames(:);
  counts = double(__categorical_codes_count__(A.codes, length(A.categoryNames)));
  if isrow(A)
    S.Counts = reshape(counts, 1, []);
  else
    S.Counts = counts(:);
  end
  tf = isundefined(A);
  S.NumMissing = sum(tf(:));
end
%=============================================================================
function localCategoricalSummary(A, varargin)
  if isempty(varargin)
    counts = __categorical_codes_count__(A.codes, length(A.categoryNames));
  else
    counts = countcats(A, varargin{1});
  end
  for k = 1:length(A.categoryNames)
    if isscalar(counts(k, :))
      fprintf('%s: %d\n', A.categoryNames{k}, counts(k));
    else
      fprintf('%s:', A.categoryNames{k});
      fprintf(' %d', counts(k, :));
      fprintf('\n');
    end
  end
  tf = isundefined(A);
  undef = sum(tf(:));
  if undef > 0
    fprintf('<undefined>: %d\n', undef);
  end
end
%=============================================================================
function value = localMedian(values)
  values = sort(values(:));
  n = length(values);
  mid = floor((n + 1) / 2);
  if mod(n, 2) == 1
    value = values(mid);
  else
    value = (values(mid) + values(mid + 1)) / 2;
  end
end
%=============================================================================
function value = localStd(values)
  values = values(:);
  n = length(values);
  if n <= 1
    value = 0;
  else
    centered = values - mean(values);
    value = sqrt(sum(centered .* centered) / (n - 1));
  end
end
%=============================================================================
function value = localMetadata(properties, fieldName, index, defaultValue)
  value = defaultValue;
  if ~isfield(properties, fieldName)
    return
  end
  metadata = properties.(fieldName);
  if isempty(metadata)
    return
  end
  if iscell(metadata) && length(metadata) >= index
    value = metadata{index};
  elseif isstring(metadata) && length(metadata) >= index
    value = char(metadata(index));
  elseif length(metadata) >= index
    value = metadata(index);
  end
end
%=============================================================================
