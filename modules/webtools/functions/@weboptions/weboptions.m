%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = weboptions(varargin)
  webtools_options = buildDefaults();
  if (mod(nargin, 2) ~= 0)
    error(_('Wrong number of input arguments.'));
  end
  for i = 1 : 2 : nargin - 1
    key = varargin{i};
    value = varargin{i + 1};
    if ~isSupportedFieldNames(key)
      error(sprintf(_('%s is not a recognized parameter.'), key));
    end
    [r, fieldname, fieldvalue] = checkArgument(key, value);
    if ~r
      error(sprintf(_('invalid value for "%s" field.'), fieldname));
    end
    webtools_options.(fieldname) = fieldvalue;
  end
  options = class(webtools_options, 'weboptions');
end
%=============================================================================
function options = buildDefaults()
  options = struct();
  options.CharacterEncoding = 'auto';
  options.UserAgent = ['NELSON', ' ', version];
  options.Timeout = 5;
  options.Username = '';
  options.Password = '';
  options.KeyName = '';
  options.KeyValue = '';
  options.ContentType = 'auto';
  options.ContentReader = [];
  options.MediaType = 'auto';
  options.RequestMethod = 'auto';
  options.ArrayFormat = 'csv';
  options.HeaderFields = [];
  certificateFilename = [modulepath('webtools'), '/resources/cacert.pem'];
  if isfile(certificateFilename)
    options.CertificateFilename = certificateFilename;
  else
    options.CertificateFilename = [];
  end
  options.FollowLocation = false;
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
  'CertificateFilename';
  'FollowLocation'};
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
    case 'FOLLOWLOCATION'
      [res, fieldname, fieldvalue] = checkFollowLocation(argValue);
    otherwise
      res = false;
      fieldname = argName;
      fieldvalue = argValue;
    end
  end
  %=============================================================================
function [res, nameNormalized, valueNormalized] = checkCharacterencoding(argValue)
  supportedCharacterEncoding = {'auto';
  'US-ASCII';
  'UTF-8';
  'latin1';
  'Shift_JIS';
  'ISO-8859-1'};
  nameNormalized = 'CharacterEncoding';
  valueNormalized = convertStringsToChars(argValue);
  idx = contains(upper(supportedCharacterEncoding), upper(valueNormalized));
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
  supportedContentType = {'text';
  'binary';
  'audio';
  'json';
  'raw';
  'auto'};
  valueNormalized = convertStringsToChars(argValue);
  idx = contains(upper(supportedContentType), upper(valueNormalized));
  res = any(idx);
  if res
    valueNormalized = supportedContentType(idx){1};
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
  supportedArrayFormat = {'csv';
  'json';
  'repeating';
  'php'};
  idx = contains(upper(supportedArrayFormat), upper(value));
  res = any(idx);
  if res
    valueNormalized = supportedArrayFormat(idx){1};
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
function [res, nameNormalized, valueNormalized] = checkFollowLocation(argValue)
  res = islogical(argValue) && isscalar(argValue) && ~issparse(argValue);
  valueNormalized = argValue;
  nameNormalized = 'FollowLocation';
end
%=============================================================================
