%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('disp'), 1);
asserts.isequal(nargout('disp'), 0);
%=============================================================================
txt = evalc('disp(''Nelson'')');
asserts.istrue(contains(txt, 'Nelson'));
%=============================================================================
txt = evalc('disp([1 2 3])');
asserts.istrue(contains(txt, '1'));
asserts.istrue(contains(txt, '2'));
asserts.istrue(contains(txt, '3'));
%=============================================================================
