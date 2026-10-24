%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = setTimeseriesName(obj, oldName, newName)
  oldName = tscollectionValidName(timeseriesText(oldName, 'Old timeseries name'));
  newName = tscollectionValidName(timeseriesText(newName, 'New timeseries name'));
  if ~isfield(obj.Members_, oldName)
    error('Nelson:time:timeseriesMemberFound', 'Timeseries member was not found.');
  end
  ts = obj.Members_.(oldName);
  ts.Name = newName;
  obj.Members_ = rmfield(obj.Members_, oldName);
  obj.Members_.(newName) = ts;
end
%=============================================================================
