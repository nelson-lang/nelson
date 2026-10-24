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
% an empty categorical array is displayed with its size
%=============================================================================
LF = char(10);
X = char(215);
format loose
%=============================================================================
g = categorical({});
REF = [LF, 'g = ', LF, LF, '  0', X, '0 empty categorical array', LF, LF];
asserts.isequal(evalc('g'), REF);
%=============================================================================
g = categorical.empty;
asserts.isequal(evalc('g'), REF);
%=============================================================================
g = categorical(zeros(0,3));
REF = [LF, 'g = ', LF, LF, '  0', X, '3 empty categorical array', LF, LF];
asserts.isequal(evalc('g'), REF);
%=============================================================================
REF = ['  0', X, '0 empty categorical array', LF, LF];
asserts.isequal(evalc('display(categorical.empty)'), REF);
%=============================================================================
asserts.isequal(evalc('disp(categorical.empty)'), '');
%=============================================================================
format compact
g = categorical(zeros(0,3));
R = evalc('g');
format loose
asserts.isequal(R, ['g = ', LF, '  0', X, '3 empty categorical array', LF]);
%=============================================================================
