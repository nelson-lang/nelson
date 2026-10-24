%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
profile('clear');
profile('on');
R = 1 + 1;
profile('clear');
p = profile('info');
status = profile('status');
profile('off');
%=============================================================================
asserts.isequal(R, 2);
asserts.istrue(isempty(p));
asserts.isequal(status.ProfilerStatus, 'on');
%=============================================================================
