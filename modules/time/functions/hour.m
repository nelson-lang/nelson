%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = hour(varargin)
  % Extracts the hour component from a given date/time input.
  %
  % Usage:
  %   h = hour(d)
  %   h = hour(d, formatDate) 
  %
  % Input:
  %   d - Date/time input (can be a string, char, or numeric).
  %   formatDate - format date (can be a string, char).
  %
  % Output:
  %   h - Hour component of the input date/time.
  
  narginchk(1, 2); 
  nargoutchk(0, 1); % Ensure at most one output argument.
  
  d = varargin{1};
  d = convertStringsToChars(d); % Convert strings to char for consistency.
  
  formatDate = '';
  if nargin == 2
    formatDate = convertStringsToChars(varargin{2});
  end
  
  % Validate input type
  if ~(ischar(d) || isstring(d) || isnumeric(d) || iscell(d))
    error('Invalid date/time format. Input must be a string, char, or numeric.');
  end
  % Convert char inputs to numeric using datenum
  if ~isnumeric(d)
    d = datenum(d, formatDate); 
  end
  
  % Convert numeric or parsed inputs to date vector
  c = datevec(d(:));
  
  % Extract hour component
  h = c(:, 4);
  
  if ~ischar(d)
    h = reshape(h, size(d)); % Reshape to match the input size if numeric.
  end
  
  varargout{1} = h; % Assign output
end
%=============================================================================

