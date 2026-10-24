%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = subsasgn(obj, S, value)
  if strcmp(S(1).type, '()')
    obj = localAssignDatetimeElements(obj, S, value);
    return
  end
  if strcmp(S(1).type, '.')
    obj = localAssignDatetimeProperty(obj, S(1).subs, value);
    return
  end
  error('Nelson:time:invalidDatetimeAssignment', 'Invalid %s.', 'datetime assignment');
end
%=============================================================================
function obj = localAssignDatetimeElements(obj, S, value)
  serial = datenum(obj);
  if isa(value, 'missing')
    serial(S(1).subs{:}) = NaN(size(value));
  else
    if isa(value, 'datetime')
      % assigned instants take the time zone of the array
      value = datetimeAlignZone(value, obj.TimeZoneValue);
    else
      value = datetime(value);
    end
    serial(S(1).subs{:}) = datenum(value);
  end
  % Elements created by growing the array are NaT, not serial day zero.
  assigned = true(size(datenum(obj)));
  assigned(S(1).subs{:}) = true;
  serial(~assigned) = NaN;
  obj.SerialValue = serial;
  obj.TimeMillis = [];
end
%=============================================================================
function obj = localAssignDatetimeProperty(obj, name, value)
  switch name
    case 'Format'
      obj.Format = value;
    case 'TimeZone'
      obj.TimeZone = value;
    case 'Year'
      obj = datetime.setComponent(obj, 1, value);
    case 'Month'
      obj = datetime.setComponent(obj, 2, value);
    case 'Day'
      obj = datetime.setComponent(obj, 3, value);
    case 'Hour'
      obj = datetime.setComponent(obj, 4, value);
    case 'Minute'
      obj = datetime.setComponent(obj, 5, value);
    case 'Second'
      obj = datetime.setComponent(obj, 6, value);
    otherwise
      error('Nelson:time:invalidDatetimeProperty', 'Invalid %s.', 'datetime property');
  end
end
%=============================================================================
