%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('date'), 0);
assert_isequal(nargout('date'), 1);
%=============================================================================
d = date();
assert_isequal(count(d, '-'), 2);
V = datevec(datenum(d));
assert_istrue(startsWith(d, num2str(V(3))));
assert_istrue(endsWith(d, num2str(V(1))));
%=============================================================================
