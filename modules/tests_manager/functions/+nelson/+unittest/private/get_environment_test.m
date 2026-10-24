%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function info = get_environment_test()
  platform = '';
  if ismac()
    platform = 'macos';
  else
    if isunix()
      platform = 'unix';
    else
      if ispc()
        platform = 'windows';
      else
        platform = 'unknown';
      end
    end
  end
  compiler = version('-compiler');
  compiler_str = [];
  for c = compiler
    compiler_str = [compiler_str, ' ', c{1}];
  end
  info = ['Nelson', ' ', version(), ' ', platform, ' ', compiler_str];
