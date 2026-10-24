%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = join(left, right, varargin)
  narginchk(2, Inf);
  mustBeA(left, 'table', 1);
  mustBeA(right, 'table', 2);
  % A simple join matches every row of the left table to a single row of the
  % right table by key. It therefore requires the right key values to be unique
  % and to cover every left key value; otherwise it errors. When both hold, the
  % result is exactly the inner join (merged keys, one row per left row, in left
  % order), so the assembly is delegated to innerjoin.
  leftSt = struct(left);
  rightSt = struct(right);
  leftNames = leftSt.Properties.VariableNames;
  rightNames = rightSt.Properties.VariableNames;
  [leftKeyNames, rightKeyNames] = resolveKeyNames(leftNames, rightNames, varargin);
  if ~isempty(rightKeyNames)
    leftKeyIdx = keyNamesToIndices(leftKeyNames, leftNames);
    rightKeyIdx = keyNamesToIndices(rightKeyNames, rightNames);
    rightRowKeys = tableRowKeys(rightSt, height(right), rightKeyIdx);
    if numel(unique(rightRowKeys)) ~= numel(rightRowKeys)
      error('Nelson:table:invalidKey', 'Right-table key values must be unique.');
    end
    leftRowKeys = tableRowKeys(leftSt, height(left), leftKeyIdx);
    tf = tableIsmemberKeys(leftRowKeys, rightRowKeys);
    if ~all(tf)
      error('Nelson:table:keyValuesNotCovered', 'Right-table key values must cover every left-table key value.');
    end
  end
  T = innerjoin(left, right, varargin{:});
end
%=============================================================================
function [leftKeyNames, rightKeyNames] = resolveKeyNames(leftNames, rightNames, args)
  leftKeyNames = {};
  rightKeyNames = {};
  k = 1;
  while k + 1 <= numel(args)
    name = args{k};
    if ischar(name) || (isstring(name) && isscalar(name))
      value = args{k + 1};
      switch lower(char(name))
        case 'keys'
          leftKeyNames = toNameCell(value);
          rightKeyNames = leftKeyNames;
        case 'leftkeys'
          leftKeyNames = toNameCell(value);
        case 'rightkeys'
          rightKeyNames = toNameCell(value);
      end
    end
    k = k + 2;
  end
  if isempty(leftKeyNames) && isempty(rightKeyNames)
    common = leftNames(ismember(leftNames, rightNames));
    leftKeyNames = common;
    rightKeyNames = common;
  elseif isempty(rightKeyNames)
    rightKeyNames = leftKeyNames;
  elseif isempty(leftKeyNames)
    leftKeyNames = rightKeyNames;
  end
end
%=============================================================================
function names = toNameCell(value)
  if ischar(value)
    names = {value};
  elseif isstring(value)
    names = cellstr(value(:)');
  elseif iscell(value)
    names = value(:)';
  else
    names = value;
  end
end
%=============================================================================
function idx = keyNamesToIndices(keyNames, allNames)
  if isnumeric(keyNames)
    idx = keyNames(:)';
    return
  end
  if islogical(keyNames)
    idx = find(keyNames(:)');
    return
  end
  idx = zeros(1, numel(keyNames));
  for k = 1:numel(keyNames)
    found = find(strcmp(allNames, keyNames{k}), 1);
    if isempty(found)
      error('Nelson:table:unrecognizedVariable', _('Unrecognized table variable name ''%s''.'), keyNames{k});
    end
    idx(k) = found;
  end
end
%=============================================================================
