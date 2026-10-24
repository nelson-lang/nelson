%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tableWarnUnitMismatch()
  % Table math combining variables with and without units.
  warning('Nelson:table:math:UnitWarning', 'Operation combines defined and undefined table variable units.');
end
%=============================================================================
