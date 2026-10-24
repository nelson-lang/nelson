%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function repro_entry()
  if ~isdeployed() || ~isfile(fullfile(ctfroot(), 'application.json'))
    error('Missing deployed application context.');
  end
  value = repro_helper(21);
  resource = fullfile(fileparts(mfilename('fullpath')), 'resource.txt');
  fprintf('%d:%s\n', value, strtrim(fileread(resource)));
end
