%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#function forced_by_pragma.txt
%#exclude forced_by_flag.txt
function auto_data_entry()
  auto_data_helper();
  fprintf('%s\n', fileread('caller-data.txt'));
  directory = fileparts(mfilename('fullpath'));
  fprintf('%s\n', fileread(fullfile(directory, 'forced_by_pragma.txt')));
  fprintf('%s\n', fileread(fullfile(directory, 'forced_by_flag.txt')));
end
