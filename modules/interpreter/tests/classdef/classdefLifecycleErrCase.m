%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefLifecycleErrCase()
  % the local dies while the error unwinds, before any catch block runs
  a = ClassdefLifecycle('errlocal');
  error('ClassdefLifecycle:err', 'boom in function');
end
