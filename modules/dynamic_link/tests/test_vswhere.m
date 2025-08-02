%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  try
    js = vswhere();
    assert_istrue(isstruct(js))
  catch
    err = lasterror();
    assert_isequal(err.message, _('vswhere not found.'));
  end
else
  assert_checkerror('vswhere', _('Not implemented on this platform.'));
end