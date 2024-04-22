%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('ContentType', 'text');
o.Timeout = 30;
R = webread('https://jsonplaceholder.typicode.com/posts/1/comments', o);
assert_istrue(ischar(R));
%=============================================================================
