%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = isSupportedPlatform(test_case)
  is_all_platforms = ~test_case.options.windows_only && ~test_case.options.mac_only && ~test_case.options.unix_only;
  if is_all_platforms
    r = true;
  else
    r = (ispc() && test_case.options.windows_only) ...
      || (ismac() && test_case.options.mac_only) ...
      || (isunix() && ~ismac() && test_case.options.unix_only);
  end
end
