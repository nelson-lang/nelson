%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function parse(obj, varargin)
  narginchk(1, Inf);
  args = varargin;
  if obj.StructExpandValue
    args = localExpandStruct(args);
  end
  required = localSchemaOfType(obj, 'required');
  optional = localSchemaOfType(obj, 'optional');
  parameters = localSchemaOfType(obj, 'parameter');
  results = struct();
  usingDefaults = {};
  unmatched = struct();
  pos = 1;

  for k = required
    if pos > numel(args)
      localError(obj, 'notEnoughInputs', _('Not enough input arguments.'));
    end
    item = obj.Schema(k);
    inputParser.validateValue(item, args{pos});
    results.(item.Name) = args{pos};
    pos = pos + 1;
  end

  for k = optional
    item = obj.Schema(k);
    if pos <= numel(args) && ~localIsParameterStart(obj, args{pos})
      if inputParser.optionalAccepts(item, args{pos})
        results.(item.Name) = args{pos};
        pos = pos + 1;
      else
        results.(item.Name) = item.Default;
        usingDefaults{end + 1} = item.Name;
      end
    else
      results.(item.Name) = item.Default;
      usingDefaults{end + 1} = item.Name;
    end
  end

  for k = parameters
    item = obj.Schema(k);
    results.(item.Name) = item.Default;
    usingDefaults{end + 1} = item.Name;
  end

  while pos <= numel(args)
    if ~inputParser.isTextScalar(args{pos})
      localError(obj, 'invalidParameterName', 'Parameter name must be text.');
    end
    if pos == numel(args)
      localError(obj, 'nameValueNotPaired', _('Name-value arguments must be paired.'));
    end
    rawName = inputParser.textScalar(args{pos}, 'Parameter name');
    value = args{pos + 1};
    [index, ambiguous] = inputParser.resolveParameter(obj, rawName);
    if ambiguous && obj.KeepUnmatchedValue
      unmatched.(rawName) = value;
    elseif ambiguous
      localError(obj, 'ambiguousParameterName', 'Ambiguous parameter name.');
    elseif isempty(index)
      if obj.KeepUnmatchedValue
        unmatched.(rawName) = value;
      else
        localError(obj, 'unrecognizedParameterName', 'Unrecognized parameter name.');
      end
    else
      item = obj.Schema(index);
      inputParser.validateValue(item, value);
      results.(item.Name) = value;
      usingDefaults = localRemoveDefault(usingDefaults, item.Name);
    end
    pos = pos + 2;
  end

  obj.ResultsValue = results;
  obj.UnmatchedValue = unmatched;
  obj.UsingDefaultsValue = usingDefaults(:)';
end
%=============================================================================
function args = localExpandStruct(args)
  if numel(args) == 1 && isstruct(args{1}) && isscalar(args{1})
    st = args{1};
    names = fieldnames(st);
    expanded = {};
    for k = 1:numel(names)
      expanded{end + 1} = names{k};
      expanded{end + 1} = st.(names{k});
    end
    args = expanded;
  end
end
%=============================================================================
function indices = localSchemaOfType(obj, kind)
  indices = [];
  for k = 1:numel(obj.Schema)
    if strcmp(obj.Schema(k).Type, kind)
      indices(end + 1) = k;
    end
  end
end
%=============================================================================
function tf = localIsParameterStart(obj, value)
  tf = false;
  if ~inputParser.isTextScalar(value)
    return
  end
  name = inputParser.textScalar(value, 'Parameter name');
  [index, ambiguous] = inputParser.resolveParameter(obj, name);
  tf = ambiguous || ~isempty(index);
end
%=============================================================================
function names = localRemoveDefault(names, name)
  keep = true(1, numel(names));
  for k = 1:numel(names)
    if strcmp(names{k}, name)
      keep(k) = false;
    end
  end
  names = names(keep);
end
%=============================================================================
function prefix = localFunctionPrefix(obj)
  if isempty(obj.FunctionNameValue)
    prefix = '';
  else
    prefix = [obj.FunctionNameValue, ': '];
  end
end
%=============================================================================
function localError(obj, reason, msg)
  error(['Nelson:validators:inputParser:', reason], [localFunctionPrefix(obj), msg]);
end
%=============================================================================
