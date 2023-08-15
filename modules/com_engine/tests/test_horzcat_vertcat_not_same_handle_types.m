%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
p = str2func('pause');
b = backgroundPool();
f = parfeval(b, p, 0, 10);
%=============================================================================
assert_checkerror('R = [b, f]', 'function backgroundPool_horzcat_FevalFuture undefined.')
%=============================================================================
assert_checkerror('R = [b; f]', _('Handles being catenated have incompatible classes.'))
%=============================================================================