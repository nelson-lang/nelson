%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/loadsavebin']);
%=============================================================================
test_saveload_file = [tempname(), '.bin'];
%=============================================================================
% sparse double
A = sparse(eye(3, 3));
savebin(test_saveload_file, 'A');
REF = A;
clear A;
loadbin(test_saveload_file);
asserts.isequal(A, REF);
%=============================================================================
% sparse double complex
A = sparse(eye(3, 3) + 2i);
savebin(test_saveload_file, 'A');
REF = A;
clear A;
loadbin(test_saveload_file);
asserts.isequal(A, REF);
%=============================================================================
% sparse logical
A = sparse(logical(eye(3, 3)));
savebin(test_saveload_file, 'A');
REF = A;
clear A;
loadbin(test_saveload_file);
asserts.isequal(A, REF);
rmfile(test_saveload_file);
%=============================================================================
