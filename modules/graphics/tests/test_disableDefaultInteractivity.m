%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:1000, sin(1:1000));
%=============================================================================
% default state is enabled
assert_isequal(char(get(ax, 'DefaultInteractivity_Hidden')), 'on');
%=============================================================================
disableDefaultInteractivity(ax);
assert_isequal(char(get(ax, 'DefaultInteractivity_Hidden')), 'off');
%=============================================================================
enableDefaultInteractivity(ax);
assert_isequal(char(get(ax, 'DefaultInteractivity_Hidden')), 'on');
%=============================================================================
% a figure is not accepted
assert_checkerror('disableDefaultInteractivity(f)', 'Input must be an axes handle.');
%=============================================================================
% a line is not accepted
l = plot(axes('Parent', f), 1:3);
assert_checkerror('disableDefaultInteractivity(l)', 'Input must be an axes handle.');
%=============================================================================
% an array of axes is not accepted
ax2 = axes('Parent', f);
assert_checkerror('disableDefaultInteractivity([ax ax2])', 'Input must be an axes handle.');
%=============================================================================
close(f);
%=============================================================================
