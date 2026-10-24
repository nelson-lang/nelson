%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [names, modified] = makeValidName(S, varargin)
  narginchk(1, 5);
  nargoutchk(0, 2);

  replacementStyle = 'underscore';
  prefix = 'x';
  if rem(length(varargin), 2) ~= 0
    error('Nelson:makeValidName:nameValueArgumentsPaired2', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(varargin)
    name = localTextScalar(varargin{k}, 'Option name');
    switch lower(name)
      case 'replacementstyle'
        replacementStyle = lower(localTextScalar(varargin{k + 1}, 'ReplacementStyle'));
        if ~any(strcmp(replacementStyle, {'underscore', 'delete', 'hex'}))
          error('Nelson:makeValidName:unsupportedReplacementStyle2', 'Unsupported replacement style.');
        end
      case 'prefix'
        prefix = localTextScalar(varargin{k + 1}, 'Prefix');
        if ~isvarname(prefix)
          error('Nelson:makeValidName:prefixAValidVariableName2', 'Prefix must be a valid variable name.');
        end
      otherwise
        error('Nelson:makeValidName:unsupportedOption2', 'Unsupported "%s" option.', name);
    end
  end

  [items, shape, kind] = localTextList(S);
  out = cell(size(items));
  modified = false(size(items));
  for k = 1:numel(items)
    out{k} = localMakeOne(items{k}, replacementStyle, prefix);
    modified(k) = ~strcmp(out{k}, items{k});
  end
  names = localFromTextList(out, shape, kind);
  modified = reshape(modified, shape);
end
%=============================================================================
function name = localMakeOne(name, replacementStyle, prefix)
  compact = '';
  capitalizeNext = false;
  for k = 1:length(name)
    ch = name(k);
    if isspace(ch)
      capitalizeNext = true;
    else
      if capitalizeNext && ch >= 'a' && ch <= 'z'
        ch = upper(ch);
      end
      compact = [compact, ch];
      capitalizeNext = false;
    end
  end

  name = '';
  for k = 1:length(compact)
    ch = compact(k);
    if localIsNameChar(ch)
      name = [name, ch];
    else
      switch replacementStyle
        case 'underscore'
          name = [name, '_'];
        case 'hex'
          name = [name, sprintf('0x%X', double(ch))];
      end
    end
  end

  if isempty(name) || ~localIsLetter(name(1))
    name = [prefix, name];
  elseif iskeyword(name)
    name = [prefix, upper(name(1)), lower(name(2:end))];
  end
  maxLength = namelengthmax();
  if length(name) > maxLength
    name = name(1:maxLength);
  end
end
%=============================================================================
function tf = localIsLetter(ch)
  tf = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
end
%=============================================================================
function tf = localIsNameChar(ch)
  tf = localIsLetter(ch) || (ch >= '0' && ch <= '9') || ch == '_';
end
%=============================================================================
function value = localTextScalar(value, label)
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ~ischar(value) || size(value, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', label);
  end
end
%=============================================================================
function [items, shape, kind] = localTextList(value)
  shape = size(value);
  if ischar(value)
    if size(value, 1) ~= 1 && ~isempty(value)
      error('Nelson:validators:mustBeRow', '%s must be a row vector.', 'Character input');
    end
    items = {value};
    shape = [1 1];
    kind = 'char';
  elseif isstring(value)
    items = cellstr(value);
    kind = 'string';
  elseif iscellstr(value)
    items = value;
    kind = 'cell';
  else
    error('Nelson:types:stringArrayCharacterVectorCell2', 'String array, character vector, or cell array of character vectors expected.');
  end
  items = items(:)';
end
%=============================================================================
function out = localFromTextList(items, shape, kind)
  switch kind
    case 'string'
      out = reshape(string(items), shape);
    case 'cell'
      out = reshape(items, shape);
    otherwise
      out = items{1};
  end
end
%=============================================================================
