%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function S = summary(T)
  narginchk(1, 1);
  mustBeA(T, 'table', 1);
  st = struct(T);
  S = struct();
  for k = 1:length(st.Properties.VariableNames)
    name = st.Properties.VariableNames{k};
    value = st.data.(name);
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
    S.(name) = info;
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
