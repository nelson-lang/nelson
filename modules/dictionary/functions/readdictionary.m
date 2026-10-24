%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = readdictionary(varargin)
  narginchk(1, inf);
  nargoutchk(0, 1);
  filename = varargin{1};
  mustBeTextScalar(filename, 1);
  options = parseOptions(varargin(2:end));
  if ~strcmpi(options.FileType, 'auto') && ~strcmpi(options.FileType, 'json')
    error('Nelson:dictionary:onlyJsonSupported', 'Only JSON dictionary files are supported.');
  end

  txt = fileread(filename);
  if options.AllowComments
    txt = stripJsonComments(txt);
  end
  pairs = parseJsonObject(txt, options);
  keys = strings(size(pairs, 1), 1);
  values = cell(size(pairs, 1), 1);
  useCellValues = false;
  valueType = '';
  for k = 1:size(pairs, 1)
    keys(k) = string(pairs{k, 1});
    value = decodeJsonValue(pairs{k, 2}, options);
    if ischar(value)
      value = string(value);
    end
    if ~isempty(options.ValueType) && ~isa(value, options.ValueType)
      value = feval(options.ValueType, value);
    end
    values{k} = value;
    if ~isscalar(value)
      useCellValues = true;
    end
    if isempty(valueType)
      valueType = class(value);
    elseif ~strcmp(valueType, class(value))
      useCellValues = true;
    end
  end

  if isempty(keys)
    d = dictionary();
  elseif useCellValues
    d = dictionary(keys, values);
  else
    d = dictionary();
    for k = 1:numel(keys)
      d(keys(k)) = values{k};
    end
  end
end
%=============================================================================
function options = parseOptions(args)
  options = struct();
  options.FileType = 'auto';
  options.ValueType = '';
  options.DictionaryNodeName = '';
  options.DictionarySelector = '';
  options.AllowTrailingCommas = true;
  options.AllowComments = true;
  options.AllowInfAndNaN = true;
  options.DateLocale = '';

  if mod(numel(args), 2) ~= 0
    error('Nelson:dictionary:nameValueExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(args)
    name = convertStringsToChars(args{k});
    value = args{k + 1};
    switch lower(name)
      case 'filetype'
        options.FileType = convertStringsToChars(value);
      case 'valuetype'
        options.ValueType = convertStringsToChars(value);
      case 'dictionarynodename'
        options.DictionaryNodeName = convertStringsToChars(value);
      case 'dictionaryselector'
        options.DictionarySelector = convertStringsToChars(value);
      case 'allowtrailingcommas'
        mustBeLogicalScalar(value, k + 2);
        options.AllowTrailingCommas = value;
      case 'allowcomments'
        mustBeLogicalScalar(value, k + 2);
        options.AllowComments = value;
      case 'allowinfandnan'
        mustBeLogicalScalar(value, k + 2);
        options.AllowInfAndNaN = value;
      case 'datelocale'
        options.DateLocale = convertStringsToChars(value);
      otherwise
        error('Nelson:dictionary:unsupportedOption', 'Unsupported option.');
    end
  end
  if ~isempty(options.DictionaryNodeName) || ~isempty(options.DictionarySelector)
    error('Nelson:readdictionary:optionNotSupportedForJson', 'DictionaryNodeName and DictionarySelector are not supported for JSON dictionary files.');
  end
end
%=============================================================================
function pairs = parseJsonObject(txt, options)
  txt = strtrim(convertStringsToChars(txt));
  if length(txt) < 2 || txt(1) ~= '{' || txt(end) ~= '}'
    error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
  end
  txt = strtrim(txt(2:end - 1));
  if isempty(txt)
    pairs = cell(0, 2);
    return
  end

  pairs = {};
  idx = 1;
  while idx <= length(txt)
    [key, idx] = parseJsonString(txt, idx);
    idx = skipBlanks(txt, idx);
    if idx > length(txt) || txt(idx) ~= ':'
      error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
    end
    idx = idx + 1;
    valueStart = skipBlanks(txt, idx);
    [valueText, idx] = parseJsonValue(txt, valueStart);
    pairs(end + 1, :) = {key, valueText};
    idx = skipBlanks(txt, idx);
    if idx <= length(txt)
      if txt(idx) ~= ','
        error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
      end
      idx = skipBlanks(txt, idx + 1);
      if idx > length(txt)
        if options.AllowTrailingCommas
          break
        else
          error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
        end
      end
    end
  end
end
%=============================================================================
function [value, idx] = parseJsonString(txt, idx)
  idx = skipBlanks(txt, idx);
  if idx > length(txt) || txt(idx) ~= '"'
    error('Nelson:readdictionary:jsonStringExpected', 'JSON string expected.');
  end
  start = idx;
  idx = idx + 1;
  escaped = false;
  while idx <= length(txt)
    ch = txt(idx);
    if escaped
      escaped = false;
    elseif ch == '\'
      escaped = true;
    elseif ch == '"'
      value = jsondecode(txt(start:idx));
      idx = idx + 1;
      return
    end
    idx = idx + 1;
  end
  error('Nelson:readdictionary:unterminatedJsonString', 'Unterminated JSON string.');
end
%=============================================================================
function [valueText, idx] = parseJsonValue(txt, idx)
  start = idx;
  depth = 0;
  inString = false;
  escaped = false;
  while idx <= length(txt)
    ch = txt(idx);
    if inString
      if escaped
        escaped = false;
      elseif ch == '\'
        escaped = true;
      elseif ch == '"'
        inString = false;
      end
    else
      if ch == '"'
        inString = true;
      elseif ch == '{' || ch == '['
        depth = depth + 1;
      elseif ch == '}' || ch == ']'
        if depth == 0
          break
        end
        depth = depth - 1;
      elseif ch == ',' && depth == 0
        break
      end
    end
    idx = idx + 1;
  end
  valueText = strtrim(txt(start:idx - 1));
end
%=============================================================================
function idx = skipBlanks(txt, idx)
  while idx <= length(txt) && (isspace(txt(idx)) || any(double(txt(idx)) == [9 10 11 12 13 32]))
    idx = idx + 1;
  end
end
%=============================================================================
function value = decodeJsonValue(valueText, options)
  trimmed = strtrim(valueText);
  if startsWith(trimmed, '[')
    value = decodeJsonArrayValue(trimmed, options);
    return
  end
  if startsWith(trimmed, '{')
    value = decodeJsonObjectValue(trimmed, options);
    return
  end
  switch trimmed
    case 'NaN'
      if ~options.AllowInfAndNaN
        error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
      end
      value = NaN;
      return
    case {'Infinity', 'Inf'}
      if ~options.AllowInfAndNaN
        error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
      end
      value = Inf;
      return
    case {'-Infinity', '-Inf'}
      if ~options.AllowInfAndNaN
        error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
      end
      value = -Inf;
      return
  end
  value = jsondecode(valueText);
  if ischar(value)
    value = string(value);
  end
end
%=============================================================================
function value = decodeJsonObjectValue(valueText, options)
  pairs = parseJsonObject(valueText, options);
  value = struct();
  for k = 1:size(pairs, 1)
    field = pairs{k, 1};
    value.(field) = decodeJsonValue(pairs{k, 2}, options);
  end
end
%=============================================================================
function value = decodeJsonArrayValue(valueText, options)
  if length(valueText) < 2 || valueText(1) ~= '[' || valueText(end) ~= ']'
    error('Nelson:readdictionary:jsonObjectExpected', 'JSON object expected.');
  end
  body = strtrim(valueText(2:end - 1));
  if isempty(body)
    value = [];
    return
  end
  parts = splitJsonList(body);
  values = cell(numel(parts), 1);
  for k = 1:numel(parts)
    values{k} = decodeJsonValue(parts{k}, options);
  end
  value = values;
end
%=============================================================================
function parts = splitJsonList(txt)
  parts = {};
  start = 1;
  idx = 1;
  depth = 0;
  inString = false;
  escaped = false;
  while idx <= length(txt)
    ch = txt(idx);
    if inString
      if escaped
        escaped = false;
      elseif ch == '\'
        escaped = true;
      elseif ch == '"'
        inString = false;
      end
    else
      if ch == '"'
        inString = true;
      elseif ch == '[' || ch == '{'
        depth = depth + 1;
      elseif ch == ']' || ch == '}'
        depth = depth - 1;
      elseif ch == ',' && depth == 0
        parts{end + 1} = strtrim(txt(start:idx - 1));
        start = idx + 1;
      end
    end
    idx = idx + 1;
  end
  parts{end + 1} = strtrim(txt(start:end));
end
%=============================================================================
function txt = stripJsonComments(txt)
  txt = convertStringsToChars(txt);
  out = '';
  idx = 1;
  inString = false;
  escaped = false;
  while idx <= length(txt)
    ch = txt(idx);
    if inString
      out(end + 1) = ch;
      if escaped
        escaped = false;
      elseif ch == '\'
        escaped = true;
      elseif ch == '"'
        inString = false;
      end
      idx = idx + 1;
    elseif ch == '"'
      inString = true;
      out(end + 1) = ch;
      idx = idx + 1;
    elseif ch == '/' && idx < length(txt) && txt(idx + 1) == '/'
      idx = idx + 2;
      while idx <= length(txt) && ~any(double(txt(idx)) == [10 13])
        idx = idx + 1;
      end
    elseif ch == '/' && idx < length(txt) && txt(idx + 1) == '*'
      idx = idx + 2;
      while idx < length(txt) && ~(txt(idx) == '*' && txt(idx + 1) == '/')
        idx = idx + 1;
      end
      idx = min(idx + 2, length(txt) + 1);
    else
      out(end + 1) = ch;
      idx = idx + 1;
    end
  end
  txt = out;
end
%=============================================================================
