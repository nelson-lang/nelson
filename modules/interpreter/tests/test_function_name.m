%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
% The filename is the callable name even when the primary function is
% declared under a different name (standard behavior).
R = tutu();
asserts.isequal(R, 'hello');
%=============================================================================
R = toto();
asserts.isequal(R, 'hello');
%=============================================================================
msg = strrep(_('Function ''%s'' has already been declared within this scope.'), '%s', 'tata');
asserts.checkerror('tata()', msg);
%=============================================================================
