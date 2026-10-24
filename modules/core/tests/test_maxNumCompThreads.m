%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('maxNumCompThreads'), -1);
asserts.isequal(nargout('maxNumCompThreads'), 1);
%=============================================================================
nb_cores = maxNumCompThreads();
asserts.istrue(nb_cores >= 0);
%=============================================================================
nb = maxNumCompThreads(10);
asserts.isequal(nb, nb_cores);
%=============================================================================
nb_cores2 = maxNumCompThreads();
asserts.isequal(nb_cores2, 10);
%=============================================================================
nb_cores3 = maxNumCompThreads('automatic');
asserts.isequal(nb_cores3, nb_cores2);
%=============================================================================
nb_cores4 = maxNumCompThreads();
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(nb_cores4 >= 1);
else
  asserts.isequal(nb_cores4, nb_cores);
end
%=============================================================================
