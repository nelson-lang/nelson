%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [insertAt, newVariableNames] = tableAddMoveOptions(names, args, acceptNewVariableNames)
  % Name-value arguments of addvars and movevars: Before/After, plus
  % NewVariableNames when accepted. Names are case-insensitive and
  % abbreviable, the last occurrence wins.
  % insertAt: position in the input table in front of which the variables
  % go (width + 1 without a location).
  % newVariableNames: [] when not given, otherwise a 1xN cell of names.
  params = {'Before', 'After'};
  if acceptNewVariableNames
    params{end + 1} = 'NewVariableNames';
  end
  if mod(length(args), 2) ~= 0
    error('Nelson:table:wrongNumberArgs', _('Incorrect number of arguments.'));
  end
  position = '';
  location = [];
  newVariableNames = [];
  for k = 1:2:length(args)
    param = localParamName(args{k}, params);
    switch param
      case 'NewVariableNames'
        newVariableNames = localNewVariableNames(args{k + 1});
      otherwise
        if ~isempty(position) && ~strcmp(position, param)
          error('Nelson:table:beforeAndAfter', _('Specify either ''Before'' or ''After'', but not both.'));
        end
        position = param;
        location = args{k + 1};
    end
  end
  insertAt = localInsertionPoint(names, position, location);
end
%=============================================================================
function param = localParamName(name, params)
  if ~((ischar(name) && isrow(name)) || (isstring(name) && isscalar(name) && ~ismissing(name) && strlength(name) > 0))
    error('Nelson:table:illegalParamName', _('Invalid parameter name. Parameter name must be a nonempty string or character vector.'));
  end
  name = char(name);
  for k = 1:length(params)
    if length(name) <= length(params{k}) && strncmpi(name, params{k}, length(name))
      param = params{k};
      return
    end
  end
  error('Nelson:table:badParamName', _('Invalid parameter name: %s.'), name);
end
%=============================================================================
function names = localNewVariableNames(names)
  if ischar(names) && (isrow(names) || isempty(names))
    names = {names};
  elseif isstring(names)
    names = cellstr(names);
  elseif ~iscellstr(names)
    error('Nelson:table:invalidVarNames', _('New variable names must be a character vector, string array, or cell array of character vectors.'));
  end
  names = reshape(names, 1, []);
end
%=============================================================================
function insertAt = localInsertionPoint(names, position, location)
  nvars = length(names);
  if isempty(position)
    insertAt = nvars + 1;
    return
  end
  isAfter = strcmp(position, 'After');
  if isnumeric(location) && isscalar(location)
    % 'After', 0 is the first slot and 'Before', width + 1 the last one:
    % a slot index is checked like a variable index of a width + 1 table.
    insertAt = real(double(tableResolveVariables(cell(1, nvars + 1), double(location) + isAfter)));
    return
  end
  idx = tableResolveVariableSubscript(names, location, 'Nelson:table:invalidLocation', ...
    _('Using a vartype subscripter as a location is not supported. Specify a location as a variable name, a positive integer, or a logical vector containing a single true value.'));
  if length(idx) ~= 1
    error('Nelson:table:nonscalarPosition', _('Specify a single location as a positive integer, a variable name, or a logical vector containing a single true value.'));
  end
  insertAt = idx + isAfter;
end
%=============================================================================
