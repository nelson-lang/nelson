%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismac()
  binpath = modulepath('nelson', 'bin');
  nelson_cli = [binpath, '/nelson-cli'];
  % '--gui' is a launcher mode selector (the 'nelson' dispatcher maps it to
  % the nelson-gui desktop), so the nelson-cli binary must reject it directly.
  [status, output] = run_nelson_cli(nelson_cli, ' --gui');
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:ModeSelector'));
end
%=============================================================================
