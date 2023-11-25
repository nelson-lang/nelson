%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = subsasgn(varargin)
  cl = varargin{1};
  S = varargin{2};
  value = varargin{3};
  if strcmp(S.type, '.') == false
    error(_('Illegal indexing structure argument: type ''.'' expected.'));
  end
  key = S.subs;
  if ~isSupportedFieldNames(key)
    error(sprintf(_('"%s" is not a recognized parameter.'), key));
  end
  [r, fieldname, fieldvalue] = checkArgument(key, value);
  if ~r
    error(sprintf(_('invalid value for "%s" field.'), fieldname));
  end
  s = struct(cl);
  s.(fieldname) = string(fieldvalue);
  r = class(s, 'DisplayFormatOptions');
end
%=============================================================================
function res = isSupportedFieldNames(argName)
  supportedOptions = {'NumericFormat'; 'LineSpacing'};
  res = any(contains(upper(supportedOptions), upper(argName)));
end
%=============================================================================
function [res, fieldname, fieldvalue] = checkArgument(argName, argValue)
  if ~ischar(convertStringsToChars(argName))
    error(_('String or characters expected.'));
  end
  switch(upper(argName))
    case 'NUMERICFORMAT'
      [res, fieldname, fieldvalue] = checkNumericFormat(argValue);
    case 'LINESPACING'
      [res, fieldname, fieldvalue] = checkLineSpacing(argValue);
    otherwise
      res = false;
      fieldname = argName;
      fieldvalue = argValue;
    end
  end
  %=============================================================================
function [res, nameNormalized, valueNormalized] = checkNumericFormat(argValue)
  supportedOptions = {'short';	
  'long';
  'shortE';
  'longE';
  'shortG';
  'longG';
  'shortEng';
  'longEng';
  '+';
  'bank';
  'hex';
  'rational'}; 
  nameNormalized = 'NumericFormat';
  valueNormalized = convertStringsToChars(argValue);
  idx = contains(upper(supportedOptions), upper(valueNormalized));
  res = any(idx);
  if res
    valueNormalized = supportedOptions(idx){1};
  else
    valueNormalized = [];
  end
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkLineSpacing(argValue)
  supportedOptions = {'compact'; 'loose'};
  nameNormalized = 'LineSpacing';
  valueNormalized = convertStringsToChars(argValue);
  idx = contains(upper(supportedOptions), upper(valueNormalized));
  res = any(idx);
  if res
    valueNormalized = supportedOptions(idx){1};
  else
    valueNormalized = [];
  end
end
%=============================================================================
