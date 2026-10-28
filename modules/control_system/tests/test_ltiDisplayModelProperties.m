%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = ss(1, 2, 3, 4, 'StateName', {'x'});
R = evalc('displayModelProperties(sys, [''Model Properties'', char(10)])');
asserts.istrue(contains(R, 'Model Properties'));
asserts.isfalse(contains(R, 'StateName:'));
%=============================================================================
R = evalc('displayModelProperties(sys, '''')');
asserts.isequal(R, '');
%=============================================================================
sys = tf(1, [1 2]);
R = evalc('displayModelProperties(sys, [''Model Properties'', char(10)])');
asserts.istrue(contains(R, 'Model Properties'));
asserts.isfalse(contains(R, 'Numerator:'));
%=============================================================================
