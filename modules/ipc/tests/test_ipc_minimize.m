%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
  sleep(2); % wait for minimize
  R = ipc(getpid, 'minimize');
  assert_isequal(R, true)
end
%=============================================================================
if ismac() || ispc()
  ipc(getpid, 'minimize', false);
  sleep(2); % wait for minimize
  R = ipc(getpid, 'minimize');
  assert_isequal(R, false)
end
%=============================================================================
