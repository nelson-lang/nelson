%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [opts, cache] = jsonTableDetect(root, rootIsArray, args, columns)
  % Detects the JSON table layout: table node, variables (JSON Pointers),
  % names and types. Returns a JSONImportOptions object and the decoded
  % rows and columns (cache) so that a following read does not redo them.
  % 'columns': see jsonTableDecode.
  opts = args.Options;
  if any(strcmp(args.Set, 'TableSelector'))
    selector = opts.TableSelector;
  elseif ~isempty(columns)
    selector = columns.Selector;
  elseif ~isempty(args.TableNodeName)
    selector = findNode(root, '', args.TableNodeName, true);
    if isempty(selector)
      error('Nelson:spreadsheet:jsonKeyNotFound', 'JSON key not found: %s', args.TableNodeName);
    end
  elseif rootIsArray
    selector = '';
  else
    selector = findNode(root, '', '', false);
  end
  cache = jsonTableCache(root, rootIsArray, selector, columns);
  if any(strcmp(args.Set, 'VariableSelectors'))
    selectors = opts.VariableSelectors;
  else
    selectors = jsonTableLeaves(cache);
    selectors = selectors(~strcmp(selectors, opts.RowNamesSelector));
  end
  cache.Columns = cell(1, numel(selectors));
  cache.Vectors = cell(1, numel(selectors));
  types = cell(1, numel(selectors));
  for k = 1:numel(selectors)
    [cache.Columns{k}, cache.Vectors{k}] = jsonTableColumn(cache, selectors{k});
    types{k} = detectType(cache.Columns{k}, cache.Vectors{k}, args.TextType);
  end
  names = variableNames(selectors, opts.VariableNamingRule);
  opts.VariableSelectors = selectors;
  opts.VariableNames = names;
  opts.VariableTypes = types;
  opts.SelectedVariableNames = names;
  opts.TableSelector = selector;
end
%=============================================================================
function pointer = findNode(value, prefix, key, byName)
  % Depth-first search of the first array of objects (or of the key 'key').
  pointer = '';
  if ~isstruct(value) || ~isscalar(value)
    return
  end
  if byName && ~isempty(jsonFieldName(value, key))
    pointer = [prefix, '/', jsonFieldName(value, key)];
    return
  end
  names = fieldnames(value);
  for k = 1:numel(names)
    child = value.(names{k});
    childPointer = [prefix, '/', names{k}];
    if ~byName && isArrayOfObjects(child)
      pointer = childPointer;
      return
    end
    pointer = findNode(child, childPointer, key, byName);
    if ~isempty(pointer)
      return
    end
  end
end
%=============================================================================
function tf = isArrayOfObjects(value)
  tf = (isstruct(value) && ~isscalar(value)) || ...
    (iscell(value) && ~isempty(value) && all(cellfun('isclass', value, 'struct')));
end
%=============================================================================
function type = detectType(values, vector, textType)
  if islogical(vector) && ~isempty(vector)
    type = 'logical';
    return
  end
  if ~isempty(vector)
    type = 'double';
    return
  end
  if isstring(values)
    type = textType;
    return
  end
  counts = cellfun('prodofsize', values);
  isDouble = cellfun('isclass', values, 'double');
  present = ~(isDouble & counts == 0);
  if ~any(present)
    type = 'double';
  elseif all(isDouble(present) | cellfun('isclass', values(present), 'logical'))
    if all(cellfun('isclass', values(present), 'logical'))
      type = 'logical';
    else
      type = 'double';
    end
  elseif all(cellfun('isclass', values(present), 'char') | cellfun('isclass', values(present), 'cell'))
    type = textType;
  else
    type = 'cell';
  end
end
%=============================================================================
function names = variableNames(selectors, namingRule)
  names = cell(1, numel(selectors));
  for k = 1:numel(selectors)
    if strcmp(selectors{k}, 'Keys')
      names{k} = 'Keys';
      continue
    end
    tokens = jsonPointerTokens(selectors{k});
    if isempty(tokens)
      names{k} = sprintf('Var%d', k);
    else
      names{k} = tokens{end};
    end
  end
  if strcmp(namingRule, 'modify')
    names = regexprep(names, '[^a-zA-Z_0-9]', '_');
  end
  for k = 2:numel(names)
    base = names{k};
    suffix = 0;
    while any(strcmp(names(1:k - 1), names{k}))
      suffix = suffix + 1;
      names{k} = sprintf('%s_%d', base, suffix);
    end
  end
end
%=============================================================================
