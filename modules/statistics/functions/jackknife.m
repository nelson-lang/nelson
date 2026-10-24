%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function jackstat = jackknife(varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);

  jackfun = varargin{1};
  if ~isa(jackfun, 'function_handle')
    error('Nelson:statistics:JACKFUNFunctionHandle', '%s must be a function handle.', 'JACKFUN');
  end
  args = varargin(2:end);
  firstNameValue = findFirstNameValue(args);
  dataArgs = args(1:firstNameValue - 1);
  nv = args(firstNameValue:end);
  if isempty(dataArgs)
    error('Nelson:statistics:leastOneDataArgumentRequired', 'At least one %s is required.', 'data argument');
  end
  if rem(length(nv), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  parseOptions(nv);

  [n, sampleMask, dataArgs] = validateData(dataArgs);
  first = statisticRow(jackfun, deleteRows(dataArgs, sampleMask, 1));
  jackstat = zeros(n, length(first));
  jackstat(1, :) = first;
  for i = 2:n
    jackstat(i, :) = statisticRow(jackfun, deleteRows(dataArgs, sampleMask, i));
  end
end
%=============================================================================
function idx = findFirstNameValue(args)
  idx = length(args) + 1;
  for k = 1:length(args)
    if isTextScalar(args{k})
      idx = k;
      return
    end
  end
end
%=============================================================================
function parseOptions(nv)
  for k = 1:2:length(nv)
    name = optionName(nv{k});
    if strcmp(name, 'options') && ~isempty(nv{k + 1}) && ~isstruct(nv{k + 1})
      error('Nelson:statistics:optionsStructure', 'Options must be a structure.');
    end
  end
end
%=============================================================================
function name = optionName(value)
  if ~isTextScalar(value)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  if strncmp('options', text, length(text))
    name = 'options';
  else
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
end
%=============================================================================
function [n, sampleMask, dataArgs] = validateData(dataArgs)
  n = [];
  sampleMask = false(1, length(dataArgs));
  for k = 1:length(dataArgs)
    value = dataArgs{k};
    if length(dataArgs) == 1 && isvector(value) && ~isscalar(value)
      dataArgs{k} = value(:);
    end
    if ~isscalar(dataArgs{k})
      rows = size(dataArgs{k}, 1);
      if isempty(n)
        n = rows;
      elseif rows ~= n
        error('Nelson:statistics:nonscalarDataArgumentsSameNumber', '%s must have the same number of rows.', 'Nonscalar data arguments');
      end
      sampleMask(k) = true;
    end
  end
  if isempty(n)
    error('Nelson:statistics:leastOneNonscalarDataArgument', 'At least one %s is required.', 'nonscalar data argument');
  end
end
%=============================================================================
function args = deleteRows(dataArgs, sampleMask, row)
  args = dataArgs;
  for k = 1:length(args)
    if sampleMask(k)
      mask = true(size(args{k}, 1), 1);
      mask(row) = false;
      args{k} = args{k}(mask, :);
    end
  end
end
%=============================================================================
function row = statisticRow(jackfun, args)
  value = feval(jackfun, args{:});
  row = value(:)';
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
