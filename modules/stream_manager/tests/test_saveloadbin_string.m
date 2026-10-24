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
A = ["NelSon";
  "is";
  "not";
  "LensOn"];
savebin(test_saveload_file, 'A');
REF = A;
clear A;
loadbin(test_saveload_file);
asserts.isequal(A, REF);
rmfile(test_saveload_file);
%=============================================================================
