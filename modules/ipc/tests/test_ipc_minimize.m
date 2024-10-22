%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Gnome and some others platforms do not have 'minimize' concept
% macOs do not have 'minimize' concept too
%=============================================================================
if ~ismac()
  ipc(getpid, 'minimize', true);
  R = ipc(getpid, 'minimize');
  assert_isequal(R, true)
end
%=============================================================================
ipc(getpid, 'minimize', false);
R = ipc(getpid, 'minimize');
assert_isequal(R, false)
%=============================================================================
