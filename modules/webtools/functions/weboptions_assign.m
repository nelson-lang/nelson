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
function r = weboptions_assign(cl, key, value)
    if ~isSupportedFieldNames(key)
      error(sprintf(_('''%s'' is not a recognized parameter.'), key));
    end
    [r, fieldname, fieldvalue] = checkArgument(key, value);
    if ~r
      error(sprintf(_('invalid value for ''%s'' field.'), fieldname));
    end
    s = struct(cl);
    s.(fieldname) = fieldvalue;
    r = class(s, 'weboptions');
end
%=============================================================================
function res = isSupportedFieldNames(argName)
  supportedOptions = {'CharacterEncoding';
    'UserAgent';
    'Timeout';
    'Username';
    'Password';
    'KeyName';
    'KeyValue';
    'HeaderFields';
    'ContentType';
    'ContentReader';
    'MediaType';
    'RequestMethod';
    'Arrayformat';
    'CertificateFilename'};
  res = any(contains(upper(supportedOptions), upper(argName)));
end
%=============================================================================
function [res, fieldname, fieldvalue] = checkArgument(argName, argValue)
  if ~ischar(convertStringsToChars(argName))
    error(_('String or characters expected.'));
  end
  switch(upper(argName))
      case 'CHARACTERENCODING'
        [res, fieldname, fieldvalue] = checkCharacterencoding(argValue);
      case 'USERAGENT'
        [res, fieldname, fieldvalue] = checkUserAgent(argValue);
      case 'TIMEOUT'
        [res, fieldname, fieldvalue] = checkTimeOut(argValue);
      case 'USERNAME'
        [res, fieldname, fieldvalue] = checkUserName(argValue);
      case 'PASSWORD'
        [res, fieldname, fieldvalue] = checkPassword(argValue);
      case 'KEYNAME'
        [res, fieldname, fieldvalue] = checkKeyName(argValue);
      case 'KEYVALUE'
        [res, fieldname, fieldvalue] = checkKeyValue(argValue);
      case 'CONTENTTYPE'
        [res, fieldname, fieldvalue] = checkContentType(argValue);
      case 'CONTENTREADER'
        [res, fieldname, fieldvalue] = checkContentReader(argValue);
      case 'MEDIATYPE'
        [res, fieldname, fieldvalue] = checkMediaType(argValue);
      case 'REQUESTMETHOD'
        [res, fieldname, fieldvalue] = checkRequestMethod(argValue);
      case 'ARRAYFORMAT'
        [res, fieldname, fieldvalue] = checkArrayFormat(argValue);
      case 'HEADERFIELDS'
        [res, fieldname, fieldvalue] = checkHeaderFields(argValue);
      case 'CERTIFICATEFILENAME'
        [res, fieldname, fieldvalue] = checkCertificateFilename(argValue);
      otherwise
         res = false;
         fieldname = argName;
         fieldvalue = argValue;
      end
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkCharacterencoding(argValue)
  supportedOptions = {'auto';
    'US-ASCII';
    'UTF-8';
    'latin1';
    'Shift_JIS';
    'ISO-8859-1'};
  nameNormalized = 'CharacterEncoding';
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
function [res, nameNormalized, valueNormalized] = checkUserAgent(argValue)
  nameNormalized = 'UserAgent';
  valueNormalized = convertStringsToChars(argValue);
  res = ischar(valueNormalized);
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkTimeOut(argValue)
  res = isnumeric(argValue) && isscalar(argValue) && (argValue > 0);
  valueNormalized = double(argValue);
  nameNormalized = 'Timeout';
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkUserName(argValue)
  nameNormalized = 'Username';
  valueNormalized = convertStringsToChars(argValue);
  res = ischar(valueNormalized);
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkPassword(argValue)
  nameNormalized = 'Password';
  valueNormalized = convertStringsToChars(argValue);
  res = ischar(valueNormalized);
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkKeyName(argValue)
  nameNormalized = 'KeyName';
  valueNormalized = convertStringsToChars(argValue);
  res = ischar(valueNormalized);
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkKeyValue(argValue)
  res =  isstring(argValue) || ischar(argValue) || isnumeric(argValue) || islogical(argValue);
  nameNormalized = 'KeyValue';
  if res
    valueNormalized = reshape(argValue, 1, numel(argValue));
  else
    valueNormalized = [];
  end
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkContentType(argValue)
  nameNormalized = 'ContentType';
  value = convertStringsToChars(argValue);
  supportedOptions = {'text';
    'binary';
    'audio';
    'json';
    'raw';
    'auto'};
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
function [res, nameNormalized, valueNormalized] = checkContentReader(argValue)
  res =  isfunction_handle(argValue) && isscalar(argValue);
  nameNormalized = 'ContentReader';
  valueNormalized = argValue;
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkMediaType(argValue)
  nameNormalized = 'MediaType';
  valueNormalized = convertStringsToChars(argValue);
  res = ischar(valueNormalized);
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkRequestMethod(argValue)
  value = convertStringsToChars(argValue);
  supportedOptions = {'get';
    'post';
    'put'
    'delete';
    'patch';
    'auto'};
  idx = contains(upper(supportedOptions), upper(value));
  res = any(idx);
  if res
    valueNormalized = supportedOptions(idx){1};
  else
   valueNormalized = [];
  end
  nameNormalized = 'RequestMethod';
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkArrayFormat(argValue)
  value = convertStringsToChars(argValue);
  supportedOptions = {'csv';
    'json';
    'repeating';
    'php'};
  idx = contains(upper(supportedOptions), upper(value));
  res = any(idx);
  if res
    valueNormalized = supportedOptions(idx){1};
  else
   valueNormalized = [];
  end
   nameNormalized = 'ArrayFormat';
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkHeaderFields(argValue)
  res =  iscellstr(argValue) || ischar(argValue) || isstring(argValue);
  nameNormalized = 'HeaderFields';
  valueNormalized = argValue;
end
%=============================================================================
function [res, nameNormalized, valueNormalized] = checkCertificateFilename(argValue)
  nameNormalized = 'CertificateFilename';
  valueNormalized = convertStringsToChars(argValue);
  res = ischar(valueNormalized);
  if res
    if strcmp(valueNormalized, 'default') == 1
      res = true;
      if isfile([modulepath('webtools'), '/resources/cacert.pem'])
        valueNormalized = [modulepath('webtools'), '/resources/cacert.pem'];
      else
        valueNormalized = [];
      end
    else
      res = isempty(valueNormalized) || isfile(valueNormalized);
    end
  end
 end
%=============================================================================
