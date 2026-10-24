%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function writedictionary(varargin)
  narginchk(2, inf);
  nargoutchk(0, 0);
  d = varargin{1};
  mustBeA(d, 'dictionary', 1);
  filename = varargin{2};
  mustBeTextScalar(filename, 2);

  options = parseOptions(varargin(3:end));
  if ~strcmpi(options.FileType, 'auto') && ~strcmpi(options.FileType, 'json')
    error('Nelson:dictionary:onlyJsonSupported', 'Only JSON dictionary files are supported.');
  end
  txt = dictionaryToJson(d, options.PrettyPrint, options.PreserveInfAndNaN);
  filewrite(filename, txt);
end
%=============================================================================
function options = parseOptions(args)
  options = struct();
  options.FileType = 'auto';
  options.PrettyPrint = true;
  options.PreserveInfAndNaN = true;

  if mod(numel(args), 2) ~= 0
    error('Nelson:dictionary:nameValueExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:numel(args)
    name = convertStringsToChars(args{k});
    value = args{k + 1};
    switch lower(name)
      case 'filetype'
        options.FileType = convertStringsToChars(value);
      case 'prettyprint'
        mustBeLogicalScalar(value, k + 2);
        options.PrettyPrint = value;
      case 'preserveinfandnan'
        mustBeLogicalScalar(value, k + 2);
        options.PreserveInfAndNaN = value;
      otherwise
        error('Nelson:dictionary:unsupportedOption', 'Unsupported option.');
    end
  end
end
%=============================================================================
function txt = dictionaryToJson(d, pretty, preserveInfAndNaN)
  if ~isConfigured(d)
    error('Nelson:writedictionary:typesNotSet', 'Unable to write a dictionary when the key and value types are not set.');
  end

  allKeys = keys(d, 'cell');
  allValues = values(d, 'cell');
  if isempty(allKeys)
    txt = "{}";
    return
  end
  if pretty
    lines = strings(numel(allKeys) + 2, 1);
    lines(1) = "{";
    for k = 1:numel(allKeys)
      suffix = ",";
      if k == numel(allKeys)
        suffix = "";
      end
      value = dictionaryJsonValueText(allValues{k}, preserveInfAndNaN);
      lines(k + 1) = "    " + jsonencode(dictionaryJsonKey(allKeys{k})) + ": " + value + suffix;
    end
    lines(end) = "}";
    txt = join(lines, newline());
  else
    parts = strings(1, numel(allKeys));
    for k = 1:numel(allKeys)
      value = dictionaryJsonValueText(allValues{k}, preserveInfAndNaN);
      parts(k) = jsonencode(dictionaryJsonKey(allKeys{k})) + ":" + value;
    end
    txt = "{" + join(parts, ",") + "}";
  end
end
%=============================================================================
function value = dictionaryJsonValue(value)
  if iscell(value) && isscalar(value)
    value = value{1};
  end
end
%=============================================================================
function txt = dictionaryJsonValueText(value, preserveInfAndNaN)
  value = dictionaryJsonValue(value);
  if preserveInfAndNaN
    txt = jsonValueText(value);
  else
    txt = jsonencode(value);
  end
end
%=============================================================================
function txt = jsonValueText(value)
  if isstruct(value)
    txt = jsonStructText(value);
  elseif iscell(value)
    txt = jsonCellText(value);
  elseif isnumeric(value)
    txt = jsonNumericText(value);
  else
    txt = jsonencode(value);
  end
end
%=============================================================================
function txt = jsonStructText(value)
  if isempty(value)
    txt = "[]";
    return
  end
  if numel(value) > 1
    parts = strings(1, numel(value));
    for k = 1:numel(value)
      parts(k) = jsonStructText(value(k));
    end
    txt = "[" + join(parts, ",") + "]";
    return
  end
  names = fieldnames(value);
  parts = strings(1, numel(names));
  for k = 1:numel(names)
    name = names{k};
    parts(k) = jsonencode(name) + ":" + jsonValueText(value.(name));
  end
  txt = "{" + join(parts, ",") + "}";
end
%=============================================================================
function txt = jsonCellText(value)
  if isempty(value)
    txt = "[]";
    return
  end
  if isvector(value)
    parts = strings(1, numel(value));
    for k = 1:numel(value)
      parts(k) = jsonValueText(value{k});
    end
    txt = "[" + join(parts, ",") + "]";
    return
  end
  rows = strings(size(value, 1), 1);
  for r = 1:size(value, 1)
    rows(r) = jsonCellText(value(r, :));
  end
  txt = "[" + join(rows, ",") + "]";
end
%=============================================================================
function txt = jsonNumericText(value)
  if isempty(value) || all(isfinite(value(:)))
    txt = jsonencode(value);
    return
  end
  if isscalar(value)
    txt = jsonNonFiniteScalarText(value);
  elseif isvector(value)
    parts = strings(1, numel(value));
    for k = 1:numel(value)
      parts(k) = jsonNumericText(value(k));
    end
    txt = "[" + join(parts, ",") + "]";
  elseif ismatrix(value)
    rows = strings(size(value, 1), 1);
    for r = 1:size(value, 1)
      rows(r) = jsonNumericText(value(r, :));
    end
    txt = "[" + join(rows, ",") + "]";
  else
    txt = jsonencode(value);
  end
end
%=============================================================================
function txt = jsonNonFiniteScalarText(value)
  if isnan(value)
    txt = "NaN";
  elseif value > 0
    txt = "Infinity";
  else
    txt = "-Infinity";
  end
end
%=============================================================================
function key = dictionaryJsonKey(value)
  if isstring(value)
    if ~isscalar(value)
      error('Nelson:writedictionary:jsonKeysMustBeScalar', 'Dictionary JSON keys must be scalar values.');
    end
    key = char(value);
  elseif ischar(value)
    key = value;
  elseif islogical(value)
    if ~isscalar(value)
      error('Nelson:writedictionary:jsonKeysMustBeScalar', 'Dictionary JSON keys must be scalar values.');
    end
    if value
      key = 'true';
    else
      key = 'false';
    end
  elseif isnumeric(value)
    if ~isscalar(value)
      error('Nelson:writedictionary:jsonKeysMustBeScalar', 'Dictionary JSON keys must be scalar values.');
    end
    key = strtrim(num2str(value, 17));
  else
    error('Nelson:writedictionary:invalidJsonKeyType', 'Dictionary JSON keys must be text, numeric, or logical scalar values.');
  end
end
%=============================================================================
