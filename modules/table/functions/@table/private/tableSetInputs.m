%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [codesA, codesB, B, options] = tableSetInputs(A, B, args)
  % Shared front end of the table set operations (ismember, intersect,
  % setdiff, union, setxor), reached when either input is a table. Both
  % inputs must be tables with the same variable names (in any order); B is
  % returned with the variable order of A. Each row is replaced by a row of
  % group codes computed jointly over A and B, so equal rows get equal codes
  % whatever their row names, and the numeric set functions with 'rows' on
  % the codes follow the sortrows order of the tables. options holds the
  % other flags ('rows' is implicit), left to the numeric set functions to
  % validate.
  if ~istable(A)
    error('Nelson:table:invalidType', '%s must be a table.', 'First input');
  end
  if ~istable(B)
    error('Nelson:table:invalidType', '%s must be a table.', 'Second input');
  end
  options = withoutRowsFlag(args);
  namesA = A.Properties.VariableNames;
  namesB = B.Properties.VariableNames;
  if numel(namesA) ~= numel(namesB) || ~all(ismember(namesA, namesB))
    error('Nelson:table:variableNamesMustMatch', '%s must match.', 'Table variable names');
  end
  if ~isequal(namesA, namesB)
    B = B(:, namesA);
  end
  nA = height(A);
  nB = height(B);
  stA = struct(A);
  stB = struct(B);
  codes = ones(nA + nB, 1);
  if ~isempty(namesA)
    codes = zeros(nA + nB, 0);
  end
  for k = 1:length(namesA)
    column = [stA.data.(namesA{k}); stB.data.(namesA{k})];
    codes = [codes, tableVariableCodes(column, nA + nB, true)];
  end
  codesA = codes(1:nA, :);
  codesB = codes(nA + 1:end, :);
end
%=============================================================================
function options = withoutRowsFlag(args)
  options = {};
  for k = 1:length(args)
    arg = args{k};
    if ~((ischar(arg) || (isstring(arg) && isscalar(arg))) && strcmpi(arg, 'rows'))
      options{end + 1} = arg;
    end
  end
end
%=============================================================================
