%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = lag(TT, n)
  narginchk(1, 2);
  if nargin < 2
    n = 1;
  end
  if ~isnumeric(n) || ~isscalar(n)
    error('Nelson:timetable:invalidValue:lagAmountANumericScalar', 'Lag amount must be a numeric scalar.');
  end
  n = double(n);
  if fix(n) ~= n
    error('Nelson:timetable:invalidValue:lagAmountIntegerScalar', 'Lag amount must be an integer scalar.');
  end
  st = struct(TT);
  tableSt = st.Table;
  for k = 1:length(tableSt.Properties.VariableNames)
    name = tableSt.Properties.VariableNames{k};
    tableSt.data.(name) = lagValueLocal(tableSt.data.(name), n);
  end
  TT = timetable.fromTableAndTimes(tableSt, st.Properties.RowTimes, st.Properties);
end
%=============================================================================
function value = lagValueLocal(value, n)
  rows = size(value, 1);
  if rows == 0 || n == 0
    return
  end
  n = min(abs(n), rows) * sign(n);
  fill = defaultRowsLocal(value, abs(n));
  if n > 0
    value = [fill; value(1:rows - n, :)];
  else
    n = abs(n);
    value = [value(n + 1:rows, :); fill];
  end
end
%=============================================================================
function value = defaultRowsLocal(sample, n)
  if isa(sample, 'datetime')
    value = NaT(n, size(sample, 2));
  elseif isa(sample, 'duration')
    value = seconds(NaN(n, size(sample, 2)));
  elseif isnumeric(sample)
    value = NaN(n, size(sample, 2));
    try
      value = cast(value, class(sample));
    catch
    end
  elseif islogical(sample)
    value = false(n, size(sample, 2));
  elseif isstring(sample)
    value = strings(n, size(sample, 2));
  elseif iscell(sample)
    value = cell(n, size(sample, 2));
  elseif ischar(sample)
    value = char(zeros(n, size(sample, 2)));
  else
    value = sample([]);
  end
end
%=============================================================================
