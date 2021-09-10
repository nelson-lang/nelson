%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
function r = DisplayFormatOptions_assign(cl, key, value)
    if ~isSupportedFieldNames(key)
      error(sprintf(_('''%s'' is not a recognized parameter.'), key));
    end
    [r, fieldname, fieldvalue] = checkArgument(key, value);
    if ~r
      error(sprintf(_('invalid value for ''%s'' field.'), fieldname));
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
