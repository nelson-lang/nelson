%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(pow2(1,1.5), 2);
asserts.isequal(pow2(1,-1.5), 0.5);
asserts.isequal(pow2(1e-300,1100), 1.3582985290493859e+31);
asserts.isequal(pow2(0,Inf), 0);
asserts.isequal(pow2(Inf,-Inf), Inf);
asserts.isequal(pow2(0.5,1024), 8.9884656743115795e+307);
asserts.isequal(pow2(-0,Inf), -0);
asserts.isequal(pow2(1,2.9), 4);
asserts.isequal(pow2(1,-2.9), 0.25);
asserts.isequal(pow2(3,-1075), 9.8813129168249309e-324);
asserts.isequal(pow2(2,-0.5), 2);
asserts.isequal(pow2(2,0.5), 2);
asserts.isequal(pow2(1,-0.5), 1);
asserts.isequal(pow2(single(0),Inf), single(0));
asserts.isequal(pow2(-Inf,-Inf), -Inf);
asserts.isequal(pow2(Inf,-1e10), Inf);
asserts.isequal(pow2([Inf 1],[-Inf -Inf]), [Inf 0]);
asserts.isequal(1 ./ pow2(-0,Inf), -Inf);
asserts.isequal(pow2(1-eps/2,1024), 1.7976931348623157e+308);
%=============================================================================
