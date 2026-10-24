%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tf = asserts . isequal(0, 0);
asserts.istrue(tf);
%=============================================================================
raised = false;
try
  eval('asserts . isequal(0,0);');
catch
  raised = true;
end
asserts.istrue(raised);
%=============================================================================
base = 2;
eval('dotPowerValue = base .^ 3;');
asserts.isequal(dotPowerValue, 8);
%=============================================================================
