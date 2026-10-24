%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef timeseries
  properties
    Events
    Name
    UserData
    DataInfo
    TimeInfo
    QualityInfo
    TreatNaNasMissing
  end

  properties (Dependent)
    Data
    Time
    Quality
    IsTimeFirst
    Length
  end

  properties (Hidden)
    Data_
    Time_
    Quality_
    IsTimeFirst_
    Version
  end

  methods
    function obj = timeseries(varargin)
      obj = localApplyTimeseriesState(obj, localTimeseriesDefaults());
      if nargin == 0
        return
      end
      if nargin == 1 && (ischar(varargin{1}) || isstring(varargin{1}))
        obj.Name = varargin{1};
        return
      end
      parsed = timeseriesParseConstructor(varargin{:});
      obj = localApplyParsedTimeseries(obj, parsed);
      obj = timeseriesRefreshMetadata(obj);
      timeseriesValidate(obj);
    end

    function value = get.Data(obj)
      value = obj.Data_;
    end

    function obj = set.Data(obj, value)
      obj.Data_ = value;
      if isempty(obj.Time_) && ~isempty(value)
        obj.Time_ = (0:timeseriesLengthFromData(value, obj.IsTimeFirst_) - 1).';
      end
      obj = timeseriesRefreshMetadata(obj);
      timeseriesValidate(obj);
    end

    function value = get.Time(obj)
      value = obj.Time_;
    end

    function obj = set.Time(obj, value)
      obj.Time_ = timeseriesNormalizeTime(value);
      obj = timeseriesRefreshMetadata(obj);
      timeseriesValidate(obj);
    end

    function value = get.Quality(obj)
      value = obj.Quality_;
    end

    function obj = set.Quality(obj, value)
      obj.Quality_ = value;
      timeseriesValidate(obj);
    end

    function value = get.IsTimeFirst(obj)
      value = obj.IsTimeFirst_;
    end

    function obj = set.IsTimeFirst(obj, value)
      obj.IsTimeFirst_ = logical(value);
      timeseriesValidate(obj);
    end

    function value = get.Length(obj)
      value = localTimeseriesLength(obj);
    end

    function tf = isequal(obj, varargin)
      tf = true;
      for k = 1:length(varargin)
        other = varargin{k};
        tf = tf && istimeseries(other) && builtin('isequal', struct(obj), struct(other));
      end
    end
  end

  methods (Static)
    function obj = loadobj(value)
      if isa(value, 'timeseries')
        obj = localValidatedTimeseries(value);
        return
      end
      if isstruct(value)
        obj = timeseries();
        obj = localLoadTimeseriesStruct(obj, value);
        return
      end
      error('Nelson:time:inputTimeseriesObjectStructure', 'Input must be a timeseries object or structure.');
    end
  end
end
%=============================================================================
function state = localTimeseriesDefaults()
  state = struct();
  state.Data = [];
  state.Time = zeros(0, 1);
  state.Quality = [];
  state.IsTimeFirst = true;
  state.Events = {};
  state.Name = '';
  state.UserData = [];
  state.DataInfo = tsdata.datametadata();
  state.TimeInfo = tsdata.timemetadata();
  state.QualityInfo = tsdata.qualmetadata();
  state.TreatNaNasMissing = true;
  state.Version = 1;
end
%=============================================================================
function obj = localApplyTimeseriesState(obj, state)
  obj.Data_ = state.Data;
  obj.Time_ = state.Time;
  obj.Quality_ = state.Quality;
  obj.IsTimeFirst_ = state.IsTimeFirst;
  obj.Events = state.Events;
  obj.Name = state.Name;
  obj.UserData = state.UserData;
  obj.DataInfo = state.DataInfo;
  obj.TimeInfo = state.TimeInfo;
  obj.QualityInfo = state.QualityInfo;
  obj.TreatNaNasMissing = state.TreatNaNasMissing;
  obj.Version = state.Version;
end
%=============================================================================
function obj = localApplyParsedTimeseries(obj, parsed)
  obj.Name = parsed.Name;
  obj.UserData = parsed.UserData;
  obj.DataInfo = parsed.DataInfo;
  obj.TimeInfo = parsed.TimeInfo;
  obj.QualityInfo = parsed.QualityInfo;
  obj.TreatNaNasMissing = parsed.TreatNaNasMissing;
  obj.IsTimeFirst_ = parsed.IsTimeFirst;
  obj.Data_ = parsed.Data;
  obj.Time_ = timeseriesNormalizeTime(parsed.Time);
  obj.Quality_ = parsed.Quality;
end
%=============================================================================
function obj = localLoadTimeseriesStruct(obj, value)
  fields = fieldnames(value);
  for k = 1:length(fields)
    obj.(fields{k}) = value.(fields{k});
  end
  if isempty(obj.TimeInfo)
    obj.TimeInfo = tsdata.timemetadata();
  end
  if isempty(obj.DataInfo)
    obj.DataInfo = tsdata.datametadata();
  end
  if isempty(obj.QualityInfo)
    obj.QualityInfo = tsdata.qualmetadata();
  end
  obj = localValidatedTimeseries(obj);
end
%=============================================================================
function obj = localValidatedTimeseries(obj)
  obj = timeseriesRefreshMetadata(obj);
  timeseriesValidate(obj);
end
%=============================================================================
function value = localTimeseriesLength(obj)
  state = struct(obj);
  value = length(state.Time_);
end
%=============================================================================
