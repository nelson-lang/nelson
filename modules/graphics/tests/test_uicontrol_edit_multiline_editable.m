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
% <--GUI MODE-->
%=============================================================================
% Non-regression: a multi-line edit uicontrol (Max - Min > 1) must accept
% keyboard input when Enable is 'on'. The typing itself cannot be exercised
% headlessly, but the String property is the value keyboard input feeds into,
% so this test drives the same set/get path and the Enable read-only contract
% that gates whether the widget takes typed characters.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 320 220]);
%=============================================================================
% Editable multi-line edit: String round-trips (char and cell forms).
e = uicontrol('Parent', f, 'Style', 'edit', 'Max', 2, 'Min', 0, ...
    'String', 'type here', 'Position', [20 120 260 70], 'Enable', 'on');
assert_isequal(e.Enable, 'on');
assert_isequal(e.String, 'type here');
e.String = 'edited line';
assert_isequal(e.String, 'edited line');
e.String = {'first'; 'second'; 'third'};
assert_isequal(e.String, {'first'; 'second'; 'third'});
drawnow();
%=============================================================================
% The three Enable states are all accepted by a multi-line edit.
e.Enable = 'inactive';
assert_isequal(e.Enable, 'inactive');
e.String = 'still settable programmatically';
assert_isequal(e.String, 'still settable programmatically');
drawnow();
e.Enable = 'off';
assert_isequal(e.Enable, 'off');
drawnow();
e.Enable = 'on';
assert_isequal(e.Enable, 'on');
drawnow();
%=============================================================================
close(f);
%=============================================================================
