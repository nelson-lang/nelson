%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = nameValueOptions(options, varargin)
  % Updates the fields of options from name-value pairs (names are case-insensitive).
  if mod(numel(varargin), 2) ~= 0
    error('Nelson:io:invalidOption', 'Options must be name-value pairs.');
  end
  names = fieldnames(options);
  for k = 1:2:numel(varargin)
    match = strcmpi(names, char(string(varargin{k})));
    if ~any(match)
      error('Nelson:io:invalidOption', 'Unknown option: %s', char(string(varargin{k})));
    end
    options.(names{match}) = varargin{k + 1};
  end
end
%=============================================================================
