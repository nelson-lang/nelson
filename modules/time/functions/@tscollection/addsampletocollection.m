%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = addsampletocollection(obj, varargin)
  [time, sharedData, memberData] = parseCollectionSample(obj, varargin{:});
  names = fieldnames(obj.Members_);
  for k = 1:length(names)
    if isempty(fieldnames(memberData))
      obj.Members_.(names{k}) = addsample(obj.Members_.(names{k}), 'Time', time, 'Data', sharedData);
    elseif isfield(memberData, names{k})
      obj.Members_.(names{k}) = addsample(obj.Members_.(names{k}), 'Time', time, 'Data', memberData.(names{k}));
    else
      obj.Members_.(names{k}) = addsample(obj.Members_.(names{k}), 'Time', time, 'Data', missingSample(obj.Members_.(names{k})));
    end
  end
  if ~isempty(names)
    obj.Time_ = obj.Members_.(names{1}).Time;
  end
  obj = tscollectionRefreshMetadata(obj);
end
%=============================================================================
function [time, sharedData, memberData] = parseCollectionSample(obj, varargin)
  time = [];
  sharedData = [];
  memberData = struct();
  names = fieldnames(obj.Members_);
  for k = 1:2:length(varargin)
    name = timeseriesText(varargin{k}, 'Property name');
    value = varargin{k + 1};
    switch lower(name)
      case 'time'
        time = value;
      case 'data'
        sharedData = value;
      otherwise
        if ~any(strcmp(name, names))
          error('Nelson:time:unknownTimeSeriesCollectionMember', 'Unknown %s.', 'time series collection member');
        end
        memberData.(name) = value;
    end
  end
  if isempty(time)
    error('Nelson:time:sampleTimeRequired', 'Sample time is required.');
  end
  if isempty(sharedData) && isempty(fieldnames(memberData))
    error('Nelson:time:sampleDataRequired', 'Sample data is required.');
  end
end
%=============================================================================
function data = missingSample(ts)
  sampleSize = getdatasamplesize(ts);
  if isempty(sampleSize)
    data = NaN;
  else
    data = NaN(sampleSize);
  end
end
%=============================================================================
