%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Gate for SUNDIALS-dependent tests. Returns whether the backend is
% available; when the environment variable NELSON_REQUIRE_SUNDIALS is set
% to '1' (as it should be on build configurations that ship the backend) a
% missing backend raises an error instead of letting the test pass
% silently by early return.
function available = odeRequireSundialsForTest()
  available = __ode_sundials_available__();
  if ~available && strcmp(getenv('NELSON_REQUIRE_SUNDIALS'), '1')
    error(_('SUNDIALS backend expected but not available.'));
  end
end
%=============================================================================
