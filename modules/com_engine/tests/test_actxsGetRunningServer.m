%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--EXCEL REQUIRED-->
%=============================================================================
assert_isequal(nargin('actxGetRunningServer'), 1);
assert_isequal(nargout('actxGetRunningServer'), 1);
%=============================================================================
h1 = actxserver('Excel.Application');
h2 = actxGetRunningServer('Excel.Application');
assert_isequal(class(h1), class(h2));
assert_isfalse(isequal(h1, h2));
delete(h2);
delete(h1);
%=============================================================================
