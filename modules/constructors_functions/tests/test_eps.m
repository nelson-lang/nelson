%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('eps'), 0);
asserts.isequal(nargout('eps'), 1);
%=============================================================================
asserts.isequal(eps(), eps(1));
asserts.istrue(eps() > 0);
asserts.istrue(eps(1) < 1);
asserts.istrue(eps(2 - eps(1)) > 0);
asserts.isequal(eps(2 - eps(1)), eps(1));
%=============================================================================
R = eps(single(1));
asserts.isequal(class(R), 'single');
asserts.istrue(R > 0);
asserts.istrue(R < single(1));
asserts.istrue(eps(single(2) - eps(single(1))) > 0);
asserts.isequal(eps(single(2) - eps(single(1))), eps(single(1)));
%=============================================================================
% spacing at the top of the range (upward nextafter saturates)
assert_isequal(eps(realmax), 2 ^ 971);
assert_istrue(eps(realmax) > 1e290);
assert_isequal(eps(realmax('single')), single(2 ^ 104));
%=============================================================================
