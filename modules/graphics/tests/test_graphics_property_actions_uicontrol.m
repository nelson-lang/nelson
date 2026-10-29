%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
u = uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'ok');
u.UserData = magic(2);
u.Tag = 'action-test';
asserts.isequal(u.UserData, magic(2));
asserts.isequal(u.Tag, 'action-test');
close(f);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
