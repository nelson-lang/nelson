%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef timemetadata
  properties
    Units
    Format
    StartDate
    UserData
  end

  properties (Dependent)
    Start
    End
    Increment
    Length
  end

  properties (Hidden)
    Time_
  end

  methods
    function obj = timemetadata(varargin)
      obj.Units = 'seconds';
      obj.Format = '';
      obj.StartDate = '';
      obj.UserData = [];
      obj.Time_ = zeros(0, 1);
      if nargin > 0
        obj = obj.applyNameValues(varargin{:});
      end
    end

    function value = get.Start(obj)
      if isempty(obj.Time_)
        value = [];
      else
        value = obj.Time_(1);
      end
    end

    function value = get.End(obj)
      if isempty(obj.Time_)
        value = [];
      else
        value = obj.Time_(end);
      end
    end

    function value = get.Increment(obj)
      if length(obj.Time_) < 2
        value = [];
      else
        d = diff(obj.Time_(:));
        if all(abs(d - d(1)) <= eps(max(1, abs(d(1)))) * 16)
          value = d(1);
        else
          value = [];
        end
      end
    end

    function value = get.Length(obj)
      value = length(obj.Time_);
    end

    function obj = setTime(obj, time)
      obj.Time_ = time(:);
    end

    function time = getData(obj)
      time = obj.Time_;
    end

    function obj = reset(obj)
      obj.Time_ = zeros(0, 1);
    end

    function value = get(obj, varargin)
      value = localTimeMetadataGet(obj, varargin{:});
    end

    function obj = set(obj, varargin)
      obj = obj.applyNameValues(varargin{:});
    end

    function display(obj)
      disp('tsdata.timemetadata');
      disp(' ');
      disp(obj);
    end

    function value = getTimeStr(obj)
      if isempty(obj.Time_)
        value = '';
      elseif isempty(obj.StartDate)
        value = [num2str(obj.Start), ' ', obj.Units];
      else
        value = datestr(datenum(obj.StartDate) + obj.Start .* localTimeUnitDays(obj.Units));
      end
    end

    function obj = timemerge(obj, other)
      if ~isa(other, 'tsdata.timemetadata')
        error('Nelson:time:inputTimeMetadata', 'Input must be time metadata.');
      end
      obj.Time_ = unique([obj.Time_(:); other.Time_(:)]);
    end
  end

  methods
    function obj = applyNameValues(obj, varargin)
      if rem(nargin - 1, 2) ~= 0
        error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
      end
      for k = 1:2:length(varargin)
        name = localTimeText(varargin{k}, 'Property name');
        switch lower(name)
          case 'units'
            obj.Units = varargin{k + 1};
          case 'format'
            obj.Format = varargin{k + 1};
          case 'startdate'
            obj.StartDate = varargin{k + 1};
          case 'userdata'
            obj.UserData = varargin{k + 1};
          otherwise
            error('Nelson:time:unsupportedTimeMetadataProperty', 'Unsupported %s.', 'time metadata property');
        end
      end
    end
  end
end
%=============================================================================
function value = localTimeMetadataGet(obj, varargin)
  names = properties(obj);
  if nargin == 1
    value = cell2struct(cell(size(names)), names);
    for k = 1:length(names)
      value.(names{k}) = obj.(names{k});
    end
    return
  end
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin > 2
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  name = localTimeText(varargin{1}, 'Property name');
  match = strcmpi(name, names);
  if ~any(match)
    error('Nelson:time:unknownTimeMetadataProperty', 'Unknown %s property.', 'time metadata');
  end
  value = obj.(names{find(match, 1)});
end
%=============================================================================
function value = localTimeText(value, name)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:time:mustBeText', '%s must be text.', name);
  end
end
%=============================================================================
function scale = localTimeUnitDays(units)
  units = lower(localTimeText(units, 'Units'));
  switch units
    case {'weeks', 'week'}
      scale = 7;
    case {'days', 'day'}
      scale = 1;
    case {'hours', 'hour'}
      scale = 1 / 24;
    case {'minutes', 'minute'}
      scale = 1 / (24 * 60);
    case {'seconds', 'second'}
      scale = 1 / (24 * 60 * 60);
    case {'milliseconds', 'millisecond'}
      scale = 1 / (24 * 60 * 60 * 1000);
    otherwise
      scale = 1 / (24 * 60 * 60);
  end
end
%=============================================================================
