%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = settimeseriesnames(obj, names)
  if ischar(names) || isstring(names)
    names = cellstr(names);
  end
  if ~iscell(names)
    error('Nelson:time:timeseriesNamesCellArrayText', '%s must be a cell array of text.', 'Timeseries names');
  end
  current = fieldnames(obj.Members_);
  if length(names) ~= length(current)
    error('Nelson:time:numberNamesMatchNumberTimeseries', 'Number of names must match the number of timeseries members.');
  end
  values = cell(length(current), 1);
  for k = 1:length(current)
    values{k} = obj.Members_.(current{k});
  end
  obj.Members_ = struct();
  for k = 1:length(names)
    name = tscollectionValidName(timeseriesText(names{k}, 'Timeseries name'));
    values{k}.Name = name;
    obj.Members_.(name) = values{k};
  end
end
%=============================================================================
