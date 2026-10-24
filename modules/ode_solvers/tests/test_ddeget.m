%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = ddeset('RelTol', 1e-5);
asserts.isequal(ddeget(options, 'RelTol'), 1e-5);
asserts.isequal(ddeget(options, 'InitialStep', 0.1), 0.1);
% two-argument call on an unset option must return [] (not error)
asserts.isequal(ddeget(options, 'InitialStep'), []);
asserts.isequal(ddeget([], 'RelTol'), []);
asserts.checkerror('ddeget(options, ''UnknownOption'')', sprintf(_('Unknown %s option name.'), 'DDE'));
%=============================================================================
