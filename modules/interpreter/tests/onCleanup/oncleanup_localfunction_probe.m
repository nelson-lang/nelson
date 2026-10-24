%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function oncleanup_localfunction_probe()
  disp('main start');
  localWithCleanup();
  disp('main end');
end
%=============================================================================
function localWithCleanup()
  c = onCleanup(@() disp('local cleanup'));
  disp('local body');
end
%=============================================================================
