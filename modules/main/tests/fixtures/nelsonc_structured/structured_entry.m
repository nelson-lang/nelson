%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function structured_entry(label)
  directory = fileparts(mfilename('fullpath'));
  value = strtrim(fileread(fullfile(directory, 'assets', 'value.txt')));
  if ~strcmp(value, 'STRUCTURED_RESOURCE') || ~isfolder(fullfile(directory, 'assets', 'empty'))
    error('Structured application resource missing.');
  end
  fprintf('STRUCTURED_VALUE=%d\n', structured_helper(21));
  fprintf('STRUCTURED_ARGUMENT=%s\n', label);
end
