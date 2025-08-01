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
format('longE')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('single(A)');
REF =  '
  3×3×2 single array

ans(:,:,1) =

               0   9.1337585e-01   2.7849823e-01
   9.0579194e-01   6.3235927e-01   5.4688150e-01
   1.2698682e-01   9.7540401e-02   9.5750684e-01


ans(:,:,2) =

   9.6488851e-01   9.5716697e-01   1.4188634e-01
   1.5761308e-01   4.8537564e-01   4.2176127e-01
   9.7059280e-01   8.0028045e-01   9.1573554e-01

';
assert_isequal(R, REF)
%=============================================================================
