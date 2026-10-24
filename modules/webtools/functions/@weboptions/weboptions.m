%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef weboptions
  properties (Dependent)
    CharacterEncoding
    UserAgent
    Timeout
    Username
    Password
    KeyName
    KeyValue
    ContentType
    ContentReader
    MediaType
    RequestMethod
    ArrayFormat
    HeaderFields
    CertificateFilename
    FollowLocation
  end

  properties (Hidden)
    CharacterEncodingValue = 'auto'
    UserAgentValue = ''
    TimeoutValue = 5
    UsernameValue = ''
    PasswordValue = ''
    KeyNameValue = ''
    KeyValueValue = ''
    ContentTypeValue = 'auto'
    ContentReaderValue = []
    MediaTypeValue = 'auto'
    RequestMethodValue = 'auto'
    ArrayFormatValue = 'csv'
    HeaderFieldsValue = []
    CertificateFilenameValue = 'default'
    FollowLocationValue = true
  end

  methods
    function obj = weboptions(varargin)
      obj.UserAgentValue = ['NELSON', ' ', version];
      if nargin == 0
        return
      end
      if mod(nargin, 2) ~= 0
        error('Nelson:webtools:nameValueExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:nargin
        [name, msg] = resolvePropertyName(varargin{k});
        if ~isempty(msg)
          error('Nelson:weboptions:invalidPropertyName', msg);
        end
        value = varargin{k + 1};
        switch name
          case 'CharacterEncoding'
            obj.CharacterEncoding = value;
          case 'UserAgent'
            obj.UserAgent = value;
          case 'Timeout'
            obj.Timeout = value;
          case 'Username'
            obj.Username = value;
          case 'Password'
            obj.Password = value;
          case 'KeyName'
            obj.KeyName = value;
          case 'KeyValue'
            obj.KeyValue = value;
          case 'ContentType'
            obj.ContentType = value;
          case 'ContentReader'
            obj.ContentReader = value;
          case 'MediaType'
            obj.MediaType = value;
          case 'RequestMethod'
            obj.RequestMethod = value;
          case 'ArrayFormat'
            obj.ArrayFormat = value;
          case 'HeaderFields'
            obj.HeaderFields = value;
          case 'CertificateFilename'
            obj.CertificateFilename = value;
          case 'FollowLocation'
            obj.FollowLocation = value;
        end
      end
    end

    function value = get.CharacterEncoding(obj)
      value = obj.CharacterEncodingValue;
    end

    function obj = set.CharacterEncoding(obj, value)
      supported = {'auto'; 'US-ASCII'; 'UTF-8'; 'latin1'; 'Shift_JIS'; 'ISO-8859-1'};
      [value, msg] = validateChoice('CharacterEncoding', value, supported);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.CharacterEncodingValue = value;
    end

    function value = get.UserAgent(obj)
      value = obj.UserAgentValue;
    end

    function obj = set.UserAgent(obj, value)
      if ischar(value) && isempty(value)
        obj.UserAgentValue = '';
        return
      end
      [value, msg] = validateText('UserAgent', value);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.UserAgentValue = value;
    end

    function value = get.Timeout(obj)
      value = obj.TimeoutValue;
    end

    function obj = set.Timeout(obj, value)
      if ~isnumeric(value) || ~isscalar(value) || value <= 0 || issparse(value)
        error('Nelson:weboptions:invalidTimeout', '%s must be a positive numeric scalar or Inf.', 'Timeout');
      end
      obj.TimeoutValue = double(value);
    end

    function value = get.Username(obj)
      value = obj.UsernameValue;
    end

    function obj = set.Username(obj, value)
      if ischar(value) && isempty(value)
        obj.UsernameValue = '';
        return
      end
      [value, msg] = validateText('Username', value);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.UsernameValue = value;
    end

    function value = get.Password(obj)
      value = obj.PasswordValue;
    end

    function obj = set.Password(obj, value)
      if ischar(value) && isempty(value)
        obj.PasswordValue = '';
        return
      end
      [value, msg] = validateText('Password', value);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.PasswordValue = value;
    end

    function value = get.KeyName(obj)
      value = obj.KeyNameValue;
    end

    function obj = set.KeyName(obj, value)
      if ischar(value) && isempty(value)
        obj.KeyNameValue = '';
        return
      end
      [value, msg] = validateText('KeyName', value);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.KeyNameValue = value;
    end

    function value = get.KeyValue(obj)
      value = obj.KeyValueValue;
    end

    function obj = set.KeyValue(obj, value)
      if ~(isstring(value) || ischar(value) || isnumeric(value) || islogical(value)) ...
          || issparse(value)
        error('Nelson:weboptions:invalidKeyValue', 'Invalid value for "%s".', 'KeyValue');
      end
      obj.KeyValueValue = reshape(value, 1, numel(value));
    end

    function value = get.ContentType(obj)
      value = obj.ContentTypeValue;
    end

    function obj = set.ContentType(obj, value)
      supported = {'text'; 'image'; 'binary'; 'table'; 'audio'; 'json'; 'xmldom'; 'raw'; 'auto'};
      [value, msg] = validateChoice('ContentType', value, supported);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.ContentTypeValue = value;
    end

    function value = get.ContentReader(obj)
      value = obj.ContentReaderValue;
    end

    function obj = set.ContentReader(obj, value)
      if ~(isempty(value) || (isfunction_handle(value) && isscalar(value)))
        error('Nelson:weboptions:invalidContentReader', '%s must be a function handle or empty.', 'ContentReader');
      end
      obj.ContentReaderValue = value;
    end

    function value = get.MediaType(obj)
      value = obj.MediaTypeValue;
    end

    function obj = set.MediaType(obj, value)
      [value, msg] = validateText('MediaType', value);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      if ~(strcmpi(value, 'auto') || contains(value, '/'))
        error('Nelson:weboptions:invalidMediaType', '%s must be "auto" or a valid type/subtype value.', 'MediaType');
      end
      if strcmpi(value, 'auto')
        value = 'auto';
      end
      obj.MediaTypeValue = value;
    end

    function value = get.RequestMethod(obj)
      value = obj.RequestMethodValue;
    end

    function obj = set.RequestMethod(obj, value)
      supported = {'get'; 'post'; 'put'; 'delete'; 'patch'; 'auto'};
      [value, msg] = validateChoice('RequestMethod', value, supported);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.RequestMethodValue = value;
    end

    function value = get.ArrayFormat(obj)
      value = obj.ArrayFormatValue;
    end

    function obj = set.ArrayFormat(obj, value)
      supported = {'csv'; 'json'; 'repeating'; 'php'};
      [value, msg] = validateChoice('ArrayFormat', value, supported);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      obj.ArrayFormatValue = value;
    end

    function value = get.HeaderFields(obj)
      value = obj.HeaderFieldsValue;
    end

    function obj = set.HeaderFields(obj, value)
      if isempty(value)
        obj.HeaderFieldsValue = [];
        return
      end
      if isstring(value)
        value = cellstr(value);
      end
      if ~iscellstr(value) || size(value, 2) ~= 2
        error('Nelson:weboptions:invalidHeaderFields', '%s must be an M-by-2 string array or cell array of character vectors.', 'HeaderFields');
      end
      obj.HeaderFieldsValue = value;
    end

    function value = get.CertificateFilename(obj)
      value = obj.CertificateFilenameValue;
    end

    function obj = set.CertificateFilename(obj, value)
      if ischar(value) && isempty(value)
        obj.CertificateFilenameValue = '';
        return
      end
      [value, msg] = validateText('CertificateFilename', value);
      if ~isempty(msg)
        error('Nelson:weboptions:invalidValue', msg);
      end
      if isempty(value)
        obj.CertificateFilenameValue = '';
        return
      end
      if strcmp(value, 'default')
        obj.CertificateFilenameValue = 'default';
        return
      end
      if ~isfile(value)
        error('Nelson:weboptions:invalidCertificateFilename', '%s must be "default", empty, or an existing file.', 'CertificateFilename');
      end
      obj.CertificateFilenameValue = value;
    end

    function value = get.FollowLocation(obj)
      value = obj.FollowLocationValue;
    end

    function obj = set.FollowLocation(obj, value)
      if ~islogical(value) || ~isscalar(value) || issparse(value)
        error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', 'FollowLocation');
      end
      obj.FollowLocationValue = value;
    end

    function st = struct(obj)
      st = builtin('struct');
      names = supportedPropertyNames();
      if numel(obj) == 0
        for k = 1:length(names)
          name = names{k};
          st.(name) = {};
        end
        st = reshape(st([]), size(obj));
        return
      end
      for elementIndex = 1:numel(obj)
        for k = 1:length(names)
          name = names{k};
          st(elementIndex).(name) = getPropertyValue(obj(elementIndex), name);
        end
      end
      st = reshape(st, size(obj));
    end
  end
end
%=============================================================================
function [canonical, msg] = resolvePropertyName(name)
  canonical = '';
  [name, msg] = validateText('Name', name);
  if ~isempty(msg)
    return
  end
  supported = supportedPropertyNames();
  matches = startsWith(upper(supported), upper(name));
  if sum(matches) == 0
    msg = sprintf(_('"%s" is not a recognized parameter.'), name);
    return
  end
  if sum(matches) > 1
    msg = sprintf(_('"%s" matches multiple parameters.'), name);
    return
  end
  index = find(matches, 1);
  canonical = supported{index};
end
%=============================================================================
function supported = supportedPropertyNames()
  supported = {'CharacterEncoding'; 'UserAgent'; 'Timeout'; 'Username'; 'Password'; ...
    'KeyName'; 'KeyValue'; 'ContentType'; 'ContentReader'; 'MediaType'; ...
    'RequestMethod'; 'ArrayFormat'; 'HeaderFields'; 'CertificateFilename'; ...
    'FollowLocation'};
end
%=============================================================================
function value = getPropertyValue(obj, name)
  switch name
    case 'CharacterEncoding'
      value = obj.CharacterEncoding;
    case 'UserAgent'
      value = obj.UserAgent;
    case 'Timeout'
      value = obj.Timeout;
    case 'Username'
      value = obj.Username;
    case 'Password'
      value = obj.Password;
    case 'KeyName'
      value = obj.KeyName;
    case 'KeyValue'
      value = obj.KeyValue;
    case 'ContentType'
      value = obj.ContentType;
    case 'ContentReader'
      value = obj.ContentReader;
    case 'MediaType'
      value = obj.MediaType;
    case 'RequestMethod'
      value = obj.RequestMethod;
    case 'ArrayFormat'
      value = obj.ArrayFormat;
    case 'HeaderFields'
      value = obj.HeaderFields;
    case 'CertificateFilename'
      value = obj.CertificateFilename;
    case 'FollowLocation'
      value = obj.FollowLocation;
  end
end
%=============================================================================
function [value, msg] = validateText(name, value)
  msg = '';
  if isstring(value)
    if ~isscalar(value)
      msg = sprintf(_('%s must be a character vector or string scalar.'), name);
      value = '';
      return
    end
    value = char(value);
  end
  if ~ischar(value) || size(value, 1) ~= 1
    msg = sprintf(_('%s must be a character vector or string scalar.'), name);
    value = '';
  end
end
%=============================================================================
function [value, msg] = validateChoice(name, value, supported)
  [value, msg] = validateText(name, value);
  if ~isempty(msg)
    return
  end
  matches = strcmpi(supported, value);
  if sum(matches) ~= 1
    msg = sprintf(_('Invalid value for "%s".'), name);
    value = '';
    return
  end
  index = find(matches, 1);
  value = supported{index};
end
%=============================================================================
