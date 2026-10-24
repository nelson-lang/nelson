%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef tscollection
  properties
    Name
    TimeInfo
  end

  properties (Dependent)
    Time
    Length
  end

  properties (Hidden)
    Time_
    Members_
  end

  methods
    function obj = tscollection(varargin)
      obj.Name = '';
      obj.Time_ = zeros(0, 1);
      obj.TimeInfo = tsdata.timemetadata();
      obj.Members_ = struct();
      if nargin == 0
        return
      end
      if iscell(varargin{1})
        obj = addCellTimeseries(obj, varargin{1});
        args = varargin(2:end);
      elseif istimeseries(varargin{1})
        obj.Time_ = varargin{1}.Time;
        obj.TimeInfo = varargin{1}.TimeInfo;
        obj = addts(obj, varargin{1});
        args = varargin(2:end);
      else
        obj.Time_ = timeseriesNormalizeTime(varargin{1});
        args = varargin(2:end);
      end
      obj = tscollectionRefreshMetadata(obj);
      obj = applyNameValues(obj, args{:});
    end

    function value = get.Time(obj)
      value = obj.Time_;
    end

    function obj = set.Time(obj, value)
      obj.Time_ = timeseriesNormalizeTime(value);
      obj = tscollectionRefreshMetadata(obj);
      names = fieldnames(obj.Members_);
      for k = 1:length(names)
        obj.Members_.(names{k}) = resample(obj.Members_.(names{k}), obj.Time_);
      end
    end

    function value = get.Length(obj)
      value = length(obj.Time_);
    end

    function n = numel(varargin)
      n = 1;
    end

    function n = length(obj)
      n = obj.Length;
    end

    function tf = isempty(obj)
      tf = isempty(fieldnames(obj.Members_));
    end

    function varargout = size(obj, varargin)
      sz = [obj.Length, length(fieldnames(obj.Members_))];
      if nargin == 1
        if nargout <= 1
          varargout{1} = sz;
        else
          for k = 1:nargout
            if k <= length(sz)
              varargout{k} = sz(k);
            else
              varargout{k} = 1;
            end
          end
        end
      else
        varargout{1} = sz(varargin{1});
      end
    end

    function n = end(obj, k, count)
      sz = size(obj);
      if count == 1
        n = obj.Length;
      elseif k <= length(sz)
        n = sz(k);
      else
        n = 1;
      end
    end

    function value = subsref(obj, s)
      if strcmp(s(1).type, '.') && isfield(obj.Members_, s(1).subs)
        value = obj.Members_.(s(1).subs);
        if length(s) > 1
          value = subsref(value, s(2:end));
        end
        return
      end
      if strcmp(s(1).type, '()')
        value = getsampleusingtime(obj, obj.Time_(s(1).subs{:}));
        if length(s) > 1
          value = subsref(value, s(2:end));
        end
        return
      end
      if strcmp(s(1).type, '.')
        value = dotReference(obj, s(1).subs);
        if length(s) > 1
          value = subsref(value, s(2:end));
        end
        return
      end
      error('Nelson:time:invalidTscollectionIndexing', 'Invalid %s.', 'tscollection indexing');
    end

    function obj = subsasgn(obj, s, value)
      if strcmp(s(1).type, '.') && istimeseries(value)
        obj = addts(obj, value, s(1).subs);
        return
      end
      if strcmp(s(1).type, '.')
        obj = dotAssign(obj, s(1).subs, value);
        return
      end
      error('Nelson:time:invalidTscollectionAssignment', 'Invalid %s.', 'tscollection assignment');
    end

    function names = fieldnames(obj)
      names = fieldnames(obj.Members_);
    end

    function disp(obj)
      collectionName = obj.Name;
      if isempty(collectionName)
        collectionName = 'unnamed';
      end
      disp(['Time Series Collection Object: ', collectionName]);
      disp(' ');
      disp('Time vector characteristics');
      disp(' ');
      if obj.Length ~= 0
        disp(['      Start time            ', tscollectionFormatTime(obj, obj.Time_(1))]);
        disp(['      End time              ', tscollectionFormatTime(obj, obj.Time_(end))]);
      end
      disp(' ');
      disp('Member Time Series Objects:');
      disp(' ');
      names = fieldnames(obj.Members_);
      for k = 1:length(names)
        member = obj.Members_.(names{k});
        memberName = member.Name;
        if isempty(memberName)
          memberName = names{k};
        end
        disp(['      ', memberName]);
      end
    end

    function display(obj, varargin)
      if nargin == 2
        name = varargin{1};
      else
        name = inputname(1);
      end
      if ~isempty(name)
        disp([name, ' =']);
        disp(' ');
      end
      disp(obj);
    end

    function value = dotReference(obj, name)
      state = struct(obj);
      switch name
        case 'Time'
          value = state.Time_;
        case 'Length'
          value = length(state.Time_);
        otherwise
          if isfield(state.Members_, name)
            value = state.Members_.(name);
          elseif isfield(state, name)
            value = state.(name);
          else
            error('Nelson:time:unknownTscollectionProperty', 'Unknown %s property.', 'tscollection');
          end
      end
    end

    function obj = dotAssign(obj, name, value)
      if istimeseries(value)
        obj = addts(obj, value, name);
        return
      end
      switch name
        case 'Time'
          obj.Time = value;
        case 'Name'
          obj.Name = timeseriesText(value, 'Name');
        case 'TimeInfo'
          obj.TimeInfo = value;
          obj = tscollectionRefreshMetadata(obj);
        otherwise
          error('Nelson:time:unknownTscollectionProperty', 'Unknown %s property.', 'tscollection');
      end
    end
  end

  methods (Static)
    function obj = loadobj(value)
      if isa(value, 'tscollection')
        obj = value;
        obj = tscollectionRefreshMetadata(obj);
        return
      end
      if isstruct(value)
        obj = tscollection();
        fields = fieldnames(value);
        for k = 1:length(fields)
          obj.(fields{k}) = value.(fields{k});
        end
        if isempty(obj.TimeInfo)
          obj.TimeInfo = tsdata.timemetadata();
        end
        if isempty(obj.Members_)
          obj.Members_ = struct();
        end
        obj = tscollectionRefreshMetadata(obj);
        return
      end
      error('Nelson:time:inputTscollectionObjectStructure', 'Input must be a tscollection object or structure.');
    end
  end
end
%=============================================================================
function obj = addCellTimeseries(obj, series)
  for k = 1:numel(series)
    ts = series{k};
    if ~istimeseries(ts)
      error('Nelson:time:inputCellArrayContainTimeseries', 'Input cell array must contain timeseries objects.');
    end
    if isempty(fieldnames(obj.Members_))
      obj.Time_ = ts.Time;
      obj.TimeInfo = ts.TimeInfo;
    end
    obj = addts(obj, ts);
  end
end
%=============================================================================
function text = tscollectionFormatTime(obj, value)
  if isa(value, 'datetime') || isa(value, 'duration')
    text = char(string(value));
    return
  end
  units = obj.TimeInfo.Units;
  if isempty(units)
    units = 'seconds';
  end
  text = [num2str(value), ' ', units];
end
%=============================================================================
function obj = applyNameValues(obj, varargin)
  if rem(length(varargin), 2) ~= 0
    error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(varargin)
    name = lower(timeseriesText(varargin{k}, 'Property name'));
    switch name
      case 'name'
        obj.Name = varargin{k + 1};
      case 'timeinfo'
        obj.TimeInfo = varargin{k + 1};
      otherwise
        error('Nelson:time:unsupportedCollectionProperty', 'Unsupported %s.', 'collection property');
    end
  end
end
%=============================================================================
