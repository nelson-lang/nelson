%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Companion setup for From_Workspace_Demo.nflow.
%
% The fromWorkspace block reads the base-workspace variable 'simin'. This
% script defines that signal, then opens the demo diagram so it can be
% simulated right away. After the run, the toWorkspace block writes the
% result (gain of 2 applied) to the base-workspace variable 'simout'.
%=============================================================================
t = (0:0.01:10)';
simin = [t, sin(2 * pi * 0.5 * t)];
%=============================================================================
example_dir = [modulepath('nflow_blocks'), '/examples/workspace'];
open_system([example_dir, '/From_Workspace_Demo.nflow']);
%=============================================================================
