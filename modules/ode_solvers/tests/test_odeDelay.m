%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
delay = odeDelay('History', 1, 'ValueDelay', 0.1, 'SlopeDelay', @(t, y) t - 0.1);
asserts.isequal(delay.History, 1);
asserts.isequal(delay.ValueDelay, 0.1);
asserts.istrue(isa(delay.SlopeDelay, 'function_handle'));
asserts.checkerror('odeDelay(''ValueDelay'', Inf)', sprintf(_('%s must be numeric or a function handle.'), 'ValueDelay'));
asserts.checkerror('odeDelay(''Unknown'', 1)', sprintf(_('Unknown %s property.'), 'delay'));
%=============================================================================
