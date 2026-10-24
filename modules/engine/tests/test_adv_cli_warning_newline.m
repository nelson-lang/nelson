%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Non-regression: the advanced CLI terminal must terminate a warning message
% with a newline, like the basic terminal. Otherwise the next output sticks to
% the warning text (for example "WTOKEN" immediately followed by "ATOKEN").
%=============================================================================
if ~ismac()
  binpath = modulepath('nelson', 'bin');
  nelson_adv_cli = [binpath, '/nelson-adv-cli'];
  script_file = [tempname(), '.m'];
  fid = fopen(script_file, 'wt');
  fprintf(fid, "warning('WTOKEN');\n");
  fprintf(fid, "disp('ATOKEN');\n");
  fclose(fid);
  [status, output] = run_nelson_cli(nelson_adv_cli, [' -f "', script_file, '"']);
  if isfile(script_file)
    rmfile(script_file);
  end
  asserts.isequal(status, 0);
  asserts.istrue(contains(output, 'WTOKEN'));
  asserts.istrue(contains(output, 'ATOKEN'));
  asserts.isfalse(contains(output, 'WTOKENATOKEN'));
end
%=============================================================================
