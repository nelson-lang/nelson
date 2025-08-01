%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
%=============================================================================
assert_isequal(nargin('COM_range'), -1);
assert_isequal(nargout('COM_range'), 1);
%=============================================================================
s = COM_range('A1:B2');
assert_istrue(s);
%=============================================================================
s = COM_range('Nel:Son');
assert_isfalse(s);
%=============================================================================
r = COM_range(3, 4);
assert_isequal(r, 'D3');
%=============================================================================
