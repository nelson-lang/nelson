%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('dec2base'), 3)
asserts.isequal(nargout('dec2base'), 1)
%=============================================================================
R = dec2base([20, 140], 10);
REF = ['020'; '140'];
asserts.isequal(R, REF);
%=============================================================================
V = 1:100;
R = base2dec(dec2base(V, 2), 2);
asserts.isequal(R, V');
%=============================================================================
V = 1:100;
for b = [2:36]
  R = base2dec(dec2base(V, b), b);
  asserts.isequal(R, V');
end
%=============================================================================
asserts.isequal(dec2base([10 15], 16), ['A'; 'F']);
%=============================================================================
asserts.checkerror('dec2base(10, 40)', _('The base must be an integer value between 2 and 36.'));
asserts.checkerror('dec2base(10, 40)', _('The base must be an integer value between 2 and 36.'), 'Nelson:elementary_functions:invalidBase');
%=============================================================================
