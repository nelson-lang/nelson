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
%=============================================================================
% Interactions is settable and round-trips; emptying it turns off the built-in
% interactions (same effect as disableDefaultInteractivity).
ax.Interactions = [];
assert_istrue(isempty(ax.Interactions));
%=============================================================================
ax.Interactions = [1 2 3];
assert_isequal(ax.Interactions, [1 2 3]);
%=============================================================================
close(f);
%=============================================================================
