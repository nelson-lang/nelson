%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
exit_code = 0;
try
  buildhelp()
catch
  r = lasterror();
  disp(_('Error:'));
  disp(r.message);
  disp(_('help build failed.'));
  exit_code = 1;
end
exit(exit_code);