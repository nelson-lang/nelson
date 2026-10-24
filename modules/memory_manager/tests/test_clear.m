%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('clear'), 1);
asserts.isequal(nargout('clear'), 0);
asserts.isequal(nargin('clearvars'), 1);
asserts.isequal(nargout('clearvars'), 0);
%=============================================================================
clearTarget = 42;
asserts.isequal(exist('clearTarget', 'var'), 1);
clear clearTarget
asserts.isequal(exist('clearTarget', 'var'), 0);
%=============================================================================
clearKeep = 1;
clearOther = 2;
clear('-regexp', '^clearOther$');
asserts.isequal(clearKeep, 1);
asserts.isequal(exist('clearOther', 'var'), 0);
clear clearKeep
%=============================================================================
clearvarsOne = 1;
clearvarsTwo = 2;
clearvarsThree = 3;
clearvars clearvarsOne clearvarsThree
asserts.isfalse(isvar('clearvarsOne'));
asserts.istrue(isvar('clearvarsTwo'));
asserts.isfalse(isvar('clearvarsThree'));
clearvars clearvarsTwo
%=============================================================================
