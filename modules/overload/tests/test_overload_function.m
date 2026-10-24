%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'gamma');
asserts.checkerror('gamma(int32(4))', msg);
asserts.checkerror('gamma(4i)', _('Input argument must be dense and real.'));
asserts.checkerror('gamma(single(4i))', _('Input argument must be dense and real.'));
addpath([nelsonroot(), '/modules/overload/tests/overload']);
asserts.isequal(gamma(int32(4)), int32(44));
asserts.isequal(gamma(4i), 42);
asserts.checkerror('gamma(single(4i))', _('Input argument must be dense and real.'));
