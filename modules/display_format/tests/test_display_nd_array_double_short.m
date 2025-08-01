%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng('default')
format('short')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

         0    0.9134    0.2785
    0.9058    0.6324    0.5469
    0.1270    0.0975    0.9575


A(:,:,2) =

    0.9649    0.9572    0.1419
    0.1576    0.4854    0.4218
    0.9706    0.8003    0.9157

';
assert_isequal(R, REF)
%=============================================================================
