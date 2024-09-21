%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
c = complexObj(3,4);
assert_isequal(class(c), 'complexObj');
c(23, 44)
c
%=============================================================================
R = evalc('c(:,1)');
REF =     'complexObj_extract:

varargout =

  1×1 cell array

    {[1×1 complexObj]}


ans = 

complexObj_disp:
real part
complexObj_extract:
:
imag part
complexObj_extract:
     1

';
%=============================================================================