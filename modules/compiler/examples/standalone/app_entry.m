%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function app_entry(input)
  narginchk(1, 1);
  value = input;
  if ischar(value) || isstring(value); value = str2double(value); end
  if ~isa(value, 'double') || ~isscalar(value) || isnan(value)
    error('A numeric command-line argument is required.');
  end
  directory = fileparts(mfilename('fullpath'));
  factor = str2double(strtrim(fileread(fullfile(directory, 'factor.txt'))));
  result = helper_value(value) * factor;
  fprintf('TUTORIAL_RESULT=%g\n', result);
end
