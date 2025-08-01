%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
rng('default')
format('longG')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('A = single(A)');
REF =  '
  3×3×2 single array

A(:,:,1) =

               0       0.9133759       0.2784982
       0.9057919       0.6323593       0.5468815
       0.1269868       0.0975404       0.9575068


A(:,:,2) =

       0.9648885        0.957167       0.1418863
       0.1576131       0.4853756       0.4217613
       0.9705928       0.8002805       0.9157355

';
assert_isequal(R, REF)
%=============================================================================
