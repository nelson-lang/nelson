%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#function "included data.txt" callback_helper
function directives_entry()
  if ~isdeployed()
    development_only();
    fileread('ignored.txt');
  end
  z_configuration();
  value = feval('callback_helper', 21);
  filename = fullfile(fileparts(mfilename('fullpath')), 'included data.txt');
  fprintf('DIRECTIVES_VALUE=%d\n', value);
  fprintf('%s\n', fileread(filename));
end
