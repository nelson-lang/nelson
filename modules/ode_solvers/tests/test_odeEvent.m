%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
event = odeEvent('EventFcn', @(t, y) y - 1, 'Direction', 'decreasing', ...
  'Response', 'stop');
asserts.istrue(isa(event.EventFcn, 'function_handle'));
asserts.isequal(event.Direction, 'decreasing');
asserts.isequal(event.Response, 'stop');
asserts.checkerror('odeEvent(''Direction'', ''sideways'')', sprintf(_('%s has an invalid value.'), 'Direction'));
asserts.checkerror('odeEvent(''EventFcn'', 1)', sprintf(_('%s must be a function handle.'), 'Value'));
%=============================================================================
