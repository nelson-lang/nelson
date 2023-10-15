%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = strjust(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  if nargin == 1
    justify = 'right'; 
  else
    justify = lower(varargin{2});
  end
  mustBeMember(justify, ["left", "right", "center"], 2);
  
  str = varargin{1};
  % Handle empty input
  if isempty(str)
    varargout{1} = str;
    return;
  end
  
  if isnumeric(str)
    str = char(str);
  end
  
  % Get the size of the input string
  [m, n] = size(str);
  
  % Initialize justifiedText as a space-filled matrix of the same size
  justifiedText = repmat(' ', m, n);
  
  % Determine the column to add spaces
  if (strcmp(justify, 'left') && ~any(str(:, 1) == ' ')) || (strcmp(justify, 'right') && ~any(str(:, n) == ' '))
    varargout{1} = char(str);
    return;
  end 
  % Find row and column indices of non-space characters
  isChar = (str ~= 0 & str ~= ' ');
  [nr, nc] = find(isChar);
  
  % Determine how to justify the text
  if strcmp(justify, 'left')
    shift = shiftLeft(isChar);
  elseif strcmp(justify, 'right')
    shift = shiftRight(isChar);
  else
    shift = shiftCenter(isChar);
  end
  
  % Calculate input and output positions for character copying
  posIn = nr + (nc - 1) * m;
  posOut = nr + (nc + shift(nr)' - 1) * m;
  % Copy characters to the justifiedText matrix
  justifiedText(posOut) = str(posIn);
  
  varargout{1} = justifiedText;
end
%=============================================================================
function shift = shiftLeft(isChar)
  % For left justification, find the leftmost non-space character in each row
  [dummy, shift] = max(isChar, [], 2);
  shift = 1 - shift;
end
%=============================================================================
function shift = shiftRight(isChar)
  % For right justification, find the rightmost non-space character in each row
  [dummy, shift] = max(fliplr(isChar), [], 2);
  shift =  shift - 1;
end
%=============================================================================
function shift = shiftCenter(isChar)
  % For center justification, find the middle point between leftmost and rightmost non-space characters
  [dummy, shiftBefore] = max(isChar, [], 2);
  [dummy, shiftAfter] = max(fliplr(isChar), [], 2);
  shift = floor((shiftAfter - shiftBefore) / 2);
end
%=============================================================================
