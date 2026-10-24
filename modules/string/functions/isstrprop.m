%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isstrprop(str, category, varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  forceCellOutput = false;
  if numel(varargin) == 2
    if strcmpi(char(varargin{1}), 'ForceCellOutput')
      forceCellOutput = logical(varargin{2});
    else
      error('Nelson:string:unsupportedOption', 'Unsupported option.');
    end
  end
  inputWasCell = iscell(str);
  if inputWasCell
    tf = cell(size(str));
    for k = 1:numel(str)
      tf{k} = isstrprop(str{k}, category);
    end
    return
  end
  if isstring(str)
    values = cellstr(str);
    tf = cell(size(values));
    for k = 1:numel(values)
      tf{k} = isstrprop(values{k}, category);
    end
    if ~forceCellOutput
      if isscalar(tf)
        tf = tf{1};
      end
    end
    return
  end
  if isnumeric(str)
    str = char(str);
  end
  category = lower(char(category));
  switch category
    case {'alpha', 'walpha'}
      tf = isletter(str);
    case {'digit', 'wdigit'}
      tf = str >= '0' & str <= '9';
    case {'alphanum', 'walphanum'}
      tf = isletter(str) | (str >= '0' & str <= '9');
    case {'space', 'wspace'}
      tf = isspace(str);
    case 'lower'
      tf = str >= 'a' & str <= 'z';
    case 'upper'
      tf = str >= 'A' & str <= 'Z';
    case 'xdigit'
      tf = (str >= '0' & str <= '9') | (lower(str) >= 'a' & lower(str) <= 'f');
    case 'punct'
      tf = ~isstrprop(str, 'alphanum') & ~isstrprop(str, 'space');
    case 'graphic'
      tf = str > ' ' & str <= char(126);
    case 'print'
      tf = str >= ' ' & str <= char(126);
    case 'cntrl'
      tf = str < ' ' | str == char(127);
    otherwise
      error('Nelson:string:unsupportedCharacterCategory', 'Unsupported character category.');
  end
  if forceCellOutput
    tf = {tf};
  end
end
%=============================================================================
