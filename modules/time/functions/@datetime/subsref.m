%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(obj, S)
  if strcmp(S(1).type, '()')
    serial = datenum(obj);
    millis = indexedPart(datetime.storedMillis(obj), S(1).subs);
    fold = indexedPart(datetime.storedFold(obj), S(1).subs);
    out = datetime.fromSerial(serial(S(1).subs{:}), obj.FormatValue, obj.TimeZoneValue, millis, fold);
    if numel(S) > 1
      varargout{1} = subsref(out, S(2:end));
    else
      varargout{1} = out;
    end
    return
  end
  if strcmp(S(1).type, '.')
    value = localDatetimeReference(obj, S(1).subs);
    if numel(S) > 1
      value = subsref(value, S(2:end));
    end
    varargout{1} = value;
    return
  end
  error('Nelson:time:invalidDatetimeIndexing', 'Invalid %s.', 'datetime indexing');
end
%=============================================================================
function part = indexedPart(part, subs)
  % indexed companion values (empty stays empty)
  if ~isempty(part)
    part = part(subs{:});
  end
end
%=============================================================================
function value = localDatetimeReference(obj, name)
  switch name
    case 'Format'
      value = datetime.resolveDisplayFormat(obj.FormatValue, datetime.rawSerial(obj));
    case 'TimeZone'
      value = obj.TimeZoneValue;
    case 'Year'
      value = datetime.component(obj, 1);
    case 'Month'
      value = datetime.component(obj, 2);
    case 'Day'
      value = datetime.component(obj, 3);
    case 'Hour'
      value = datetime.component(obj, 4);
    case 'Minute'
      value = datetime.component(obj, 5);
    case 'Second'
      value = datetime.component(obj, 6);
    case {'SerialValue', 'FormatValue', 'TimeZoneValue'}
      state = builtin('struct', obj);
      value = state.(name);
    otherwise
      error('Nelson:time:unknownProperty', ['Invalid datetime property: ', name]);
  end
end
%=============================================================================
