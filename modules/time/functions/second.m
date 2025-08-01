%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = second(varargin)
  % Extracts the second component from a given date/time input.
  %
  % Usage:
  %   s = second(d)
  %   s = second(d, formatDate) 
  %
  % Input:
  %   d - Date/time input (can be a string, char, or numeric).
  %   formatDate - format date (can be a string, char).
  %
  % Output:
  %   s - Second component of the input date/time.
  
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  inputDate = varargin{1};
  inputDate = convertStringsToChars(inputDate);
  
  dateFormat = '';
  if nargin == 2
    dateFormat = convertStringsToChars(varargin{2});
  end
  
  if ischar(inputDate)
    inputDate = datenum(inputDate, dateFormat);
    inputSize = size(inputDate);
  elseif iscell(inputDate)
    inputSize = size(inputDate);
    inputDate = datenum(inputDate, dateFormat);
  elseif isnumeric(inputDate)
    inputSize = size(inputDate);
  else
    error(_('Invalid date/time format. Input must be a string, char, or numeric.'));
  end
  
  dateVector = datevec(inputDate(:));
  secondValues = dateVector(:, 6);
  
  secondValues = round(1000 .* secondValues) ./ 1000;
  varargout{1} = reshape(secondValues, inputSize);
end
%=============================================================================
