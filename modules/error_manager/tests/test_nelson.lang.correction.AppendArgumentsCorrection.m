%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
correction = nelson.lang.correction.AppendArgumentsCorrection({'x', 1});
%=============================================================================
asserts.isequal(class(correction), 'nelson.lang.correction.AppendArgumentsCorrection');
asserts.isequal(correction.Arguments, {'x', 1});
asserts.checkerror('correction.Arguments = ''y'';', _('Cannot set property: Arguments'));
%=============================================================================
