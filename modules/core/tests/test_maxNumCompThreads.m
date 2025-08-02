%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('maxNumCompThreads'), -1);
assert_isequal(nargout('maxNumCompThreads'), 1);
%=============================================================================
nb_cores = maxNumCompThreads();
assert_istrue(nb_cores >= 0);
%=============================================================================
nb = maxNumCompThreads(10);
assert_isequal(nb, nb_cores);
%=============================================================================
nb_cores2 = maxNumCompThreads();
assert_isequal(nb_cores2, 10);
%=============================================================================
nb_cores3 = maxNumCompThreads('automatic');
assert_isequal(nb_cores3, nb_cores2);
%=============================================================================
nb_cores4 = maxNumCompThreads();
assert_isequal(nb_cores4, nb_cores);
%=============================================================================