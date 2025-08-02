%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[status, message] = configuremsvc();
if status == true
  assert_isequal(message, _('msvc compiler detected and configured.'));
else
  assert_isequal(message, _('Not implemented on this platform.'));
end
%=============================================================================